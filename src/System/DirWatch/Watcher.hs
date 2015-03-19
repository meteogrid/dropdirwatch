{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch.Watcher (
    WatcherState
  , StopCond
  , runWatchLoop
  , mkStopCond
  , endLoop
  , processWatcher
) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (try, finally)
import Control.Monad (forM_, forM, void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), asks, ReaderT, runReaderT)
import Control.Monad.State.Strict (
    MonadState (get,put)
  , StateT
  , runStateT
  )
import Data.Conduit.Binary (sourceFile)
import Data.Default (def)
import Data.Monoid (Monoid(..))
import Data.List (intercalate, foldl')
import Data.Fixed (Fixed, E2)
import Data.HashMap.Strict as HM (
    HashMap
  , insertWith
  , unionWith
  , empty
  , elems
  , toList
  , fromList
  , fromListWith
  )
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime, getCurrentTime, utctDay, utctDayTime)
import System.INotify (
    INotify
  , Event (..)
  , EventVariety (..)
  , withINotify
  , addWatch
  )
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Files (getFileStatus, modificationTime)
import System.IO.Error (tryIOError)
import System.DirWatch.Config (
    RunnableConfig
  , RunnableWatcher
  , Config(..)
  , Watcher(..)
  , getCompiled
  )
import System.DirWatch.Util (
    AbsPath
  , joinAbsPath
  , globMatch
  , takePatternDirectory
  , archiveDestination
  , toFilePath
  , absPathsMatching
  )

import System.DirWatch.Threading (
    SomeThreadHandle
  , toSomeThreadHandle
  , forkChild
  , waitChild
  , tryWaitSomeChild
  )
import System.DirWatch.Logging (
    LoggingT
  , MonadLogger
  , runStderrLoggingT
  , logInfo
  , logError
  , logWarn
  , fromStrings
  )
import System.DirWatch.Processor (
    ProcessorM
  , ProcessorConfig (..)
  , ProcessorError
  , runProcessorM
  )
import System.DirWatch.PreProcessor (runPreProcessor, yieldFilePath)

type StopCond = MVar ()
type ThreadMap = HashMap AbsPath [FileProcessor]
type DirMap = HashMap AbsPath [RunnableWatcher]

data FileProcessor
  = FileProcessor {
      fpHandle  :: SomeThreadHandle ProcessorError
    , fpWatcher :: RunnableWatcher
    , fpStart   :: POSIXTime
  }

watchedEvents :: [EventVariety]
watchedEvents = [MoveIn, CloseWrite]

data ChanMessage
  = Work [RunnableWatcher] AbsPath
  | WakeUp
  | Finish
  deriving Show

data WatcherEnv
  = WatcherEnv {
      wConfig   :: RunnableConfig
    , wChan     :: Chan ChanMessage
    , wInotify  :: INotify
    , wDirMap   :: DirMap
  }

data FailedWatcher
  = FailedWatcher {
      fwWatcher     :: RunnableWatcher
    , fwFilename    :: FilePath
    , fwFailures    :: [(POSIXTime,ProcessorError)]
    }

data WatcherState
  = WatcherState {
      wThreads :: ThreadMap
    , wFailed  :: [FailedWatcher]
  }
instance Monoid WatcherState where
  mempty  = WatcherState empty mempty
  mappend a b
   = WatcherState {
       wThreads = unionWith (++) (wThreads a) (wThreads b)
     , wFailed  = mappend (wFailed a) (wFailed b)
   }

newtype WatcherM a
  = WatcherM {
      unWatcherM :: LoggingT (ReaderT WatcherEnv (StateT WatcherState IO)) a
      }
  deriving ( Functor, Applicative, Monad, MonadLogger, MonadReader WatcherEnv
           , MonadState WatcherState, MonadIO, MonadBase IO)

instance MonadBaseControl IO WatcherM where
   type StM WatcherM a = (a, WatcherState)
   liftBaseWith f = do
     env <- ask
     state <- get
     liftIO $ f (runWatcherEnv env state)
   restoreM (a, state) = put state >> return a

runWatcherEnv
  :: WatcherEnv -> WatcherState -> WatcherM a -> IO (a, WatcherState)
runWatcherEnv env state
  = flip runStateT state
  . flip runReaderT env
  . runStderrLoggingT
  . unWatcherM

runWatchLoop
  :: RunnableConfig -> Maybe WatcherState -> StopCond -> IO WatcherState
runWatchLoop cfg mState stopCond = withINotify $ \ino -> do
  let dirMap = mkDirMap (cfgWatchers cfg)
  env <- WatcherEnv <$> pure cfg <*> newChan <*> pure ino <*> pure dirMap
  let state = fromMaybe mempty mState
  loopth <- forkChild $ runWatcherEnv env state
                        (setupWatches >> checkExistingFiles >> loop)
  takeMVar stopCond
  writeChan (wChan env) Finish
  fmap snd $ waitChild loopth

mkDirMap :: [RunnableWatcher] -> DirMap
mkDirMap ws = foldl' go empty $ [(w,p) | w<-ws, p<-wPaths w]
  where go m (w,p) = insertWith (++) (takePatternDirectory p) [w] m

endLoop :: StopCond -> IO ()
endLoop mvar = putMVar mvar ()

mkStopCond :: IO StopCond
mkStopCond = newEmptyMVar

setupWatches :: WatcherM ()
setupWatches = do
  dir_watchers <- asks (toList . wDirMap)
  chan <- asks wChan
  forM_ dir_watchers $ \(baseDir, watchers) -> do
    let names = intercalate ", " $ map wName watchers
        go filePath = do
          let matchedWatchers = filter (any (p `globMatch`) . wPaths) watchers
              p = joinAbsPath baseDir [filePath]
          when (not (null matchedWatchers)) $
            writeChan chan $ Work matchedWatchers p
    mError <- addIWatch baseDir $ \case
                MovedIn{isDirectory=False, ..}       -> go filePath
                Closed{ wasWriteable=True, isDirectory=False
                      , maybeFilePath=Just filePath} -> go filePath
                _ -> return ()
    case mError of
      Just e ->
        $(logWarn) $ fromStrings ["Could not watch ", show baseDir, " for "
                                 , names, ": ", show e]
      Nothing ->
        $(logInfo) $ fromStrings ["Watching ", show baseDir, " for ", names]

loop :: WatcherM ()
loop = do
  handleFinishedFiles
  handleRetries
  msg <- getMessage
  case msg of
    Work ws file -> runWatchersOnFile ws file >> loop
    WakeUp       -> loop
    Finish       -> return ()

checkExistingFiles :: WatcherM ()
checkExistingFiles = do
  dir_watchers <- asks wDirMap
  waitSecs <- asks (cfgWaitSeconds . wConfig)
  chan <- asks wChan
  void $ liftIO $ forkChild $ do
    existingMap <- fmap (fromListWith (++) . concat . concat . concat) $
      forM (elems dir_watchers) $ \watchers -> do
        forM watchers $ \watcher -> do
          forM (wPaths watcher) $ \globPattern ->  do
            paths <- absPathsMatching globPattern
            forM paths $ \abspath -> return (abspath, [watcher])
    existing <- forM (toList existingMap) $ \(abspath, watchers) -> do
      mt <- fmap modificationTime $ getFileStatus (toFilePath abspath)
      return (mt, watchers, abspath)
    threadDelay  $ 1000000 * waitSecs
    forM_ existing $ \(modTime, watchers, abspath) -> do
      modTime' <- fmap modificationTime (getFileStatus (toFilePath abspath))
      when (modTime' == modTime) $ do
        -- file has not been modified, assume it is stable and work on it.
        -- If it has been modified we can ignore it assuming we'll be notified
        -- when it has been closed
        runStderrLoggingT $ $(logInfo) $
          fromStrings ["File ", show abspath, " has been stable"]
        writeChan chan $ Work watchers abspath
      

addIWatch :: AbsPath -> (Event -> IO ()) -> WatcherM (Maybe IOError)
addIWatch dir wch = do
  ino <- asks wInotify
  result <- liftIO . try $ addWatch ino watchedEvents (toFilePath dir) wch
  either (return . Just) (const (return Nothing)) result


getMessage :: WatcherM ChanMessage
getMessage = asks wChan >>= liftIO . readChan

handleFinishedFiles :: WatcherM ()
handleFinishedFiles = do
  state@WatcherState{wThreads=ts} <- get
  let running = HM.toList ts
  remaining <- fmap catMaybes $ forM running $ \(fname, runningPs) -> do
    remainingPs <- fmap catMaybes $ forM runningPs $ \fp -> do
      result <- liftIO (tryWaitSomeChild (fpHandle fp))
      case result of
        Nothing      -> return (Just fp)
        Just Nothing -> return Nothing
        Just (Just err) ->
          registerFailure fname (fpWatcher fp) err >> return Nothing
    if null remainingPs
      then do
        $(logInfo) $ fromStrings ["Finished processing ", show fname]
        archiveFile fname
        return Nothing
      else return (Just (fname, remainingPs))
  put state {wThreads=HM.fromList remaining}

archiveFile :: AbsPath -> WatcherM ()
archiveFile fname = do
  mArchiveDir <- asks (cfgArchiveDir . wConfig)
  case mArchiveDir of
    Just archiveDir -> do
      time <- liftIO getCurrentTime
      let dest = toFilePath (archiveDestination archiveDir (utctDay time) fname)
          destDir = takeDirectory dest
      exists <- liftIO $ doesFileExist dest
      let finalDest
            | exists    = dest ++ "." ++ show secs
            | otherwise = dest
          secs = realToFrac (utctDayTime time) :: Fixed E2
      result <- liftIO . tryIOError $ do
        createDirectoryIfMissing True destDir
        renameFile (toFilePath fname) finalDest
      case result of
        Left e -> do
          $(logError) $ fromStrings [ "Could not archive ", show fname, ": "
                                    , show e]
        Right () ->
          $(logInfo) $ fromStrings ["Archived ", show fname, " -> ", finalDest]
    Nothing -> return ()

handleRetries :: WatcherM ()
handleRetries = return () -- TODO

registerFailure :: AbsPath -> RunnableWatcher -> ProcessorError -> WatcherM ()
registerFailure fname wch err = do
  $(logError) $ fromStrings  [wName wch, " failed on ", show fname, ": "
                            , show err]
  return () -- TODO

              
runWatchersOnFile :: [RunnableWatcher] -> AbsPath -> WatcherM ()
runWatchersOnFile watchers filename = do
  cfg <- processorConfig
  chan <- asks wChan
  start <- liftIO getPOSIXTime
  fps <- forM watchers $ \wch -> do
    let proc = processWatcher (posixSecondsToUTCTime start) filename wch
    liftIO $ do
      th <- forkChild $ finally (runProcessorM cfg proc) (writeChan chan WakeUp)
      return FileProcessor { fpHandle  = toSomeThreadHandle th
                           , fpStart   = start
                           , fpWatcher = wch}
  addToRunningProcessors filename fps
  where

processWatcher :: UTCTime -> AbsPath -> RunnableWatcher -> ProcessorM ()
processWatcher now abspath Watcher{..} = do
  $(logInfo) $ fromStrings ["Running ", wName, " on ", show abspath]
  case wProcessor of
    Just compiled  -> do
      let preprocessor = maybe yieldFilePath getCompiled wPreProcessor
          preprocess   = runPreProcessor source now (preprocessor filepath)
          process      = uncurry $ getCompiled compiled
          source       = sourceFile filepath
          filepath     = toFilePath abspath
      preprocess >>= mapM_ process
      $(logInfo) $ fromStrings ["Finished \"", wName, "\" on ", show abspath]
    Nothing -> $(logInfo) $ fromStrings [wName, " has no processor"]


processorConfig :: WatcherM ProcessorConfig
processorConfig = do
  env <- asks (cfgShellEnv . wConfig)
  return $ def {pShellEnv = env}


addToRunningProcessors
  :: AbsPath
  -> [FileProcessor]
  -> WatcherM ()
addToRunningProcessors path fps = do
  s@WatcherState{wThreads=ts} <- get
  put $ s {wThreads = insertWith (++) path fps ts}
