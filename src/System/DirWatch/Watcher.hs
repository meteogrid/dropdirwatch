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

import Control.Arrow (second)
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (try, finally)
import Control.Monad (forM_, forM, void, when, join)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), asks, ReaderT, runReaderT)
import Control.Monad.State.Strict (MonadState (get,put), StateT, runStateT)
import Data.Conduit.Binary (sourceFile)
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Monoid (Monoid(..))
import Data.List (intercalate, foldl', find)
import Data.Fixed (Fixed, E2)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashSet as HS (union, singleton, toList, fromList)
import Data.HashMap.Strict as HM (
  HashMap, insertWith, unionWith, empty, elems, toList, fromList, fromListWith)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (
  NominalDiffTime, UTCTime, getCurrentTime, utctDay, utctDayTime)
import System.INotify (
    INotify, Event (..), EventVariety (..), withINotify, addWatch)
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Files (getFileStatus, modificationTime)
import System.IO.Error (tryIOError)
import System.DirWatch.Config (
    RunnableConfig, RunnableWatcher, Config(..), Watcher(..), WatchedPath (..))
import System.DirWatch.Util (
    AbsPath, joinAbsPath, globMatch, takePatternDirectory, archiveDestination
  , toFilePath, absPathsMatching)

import System.DirWatch.Threading (
    SomeThreadHandle, toSomeThreadHandle, forkChild, waitChild
  , tryWaitSomeChild)
import System.DirWatch.Logging (
    LoggingT, MonadLogger, runStderrLoggingT, logDebug, logInfo, logError
  , logWarn, fromStrings)
import System.DirWatch.Processor (
    ProcessorM, ProcessorConfig (..), ProcessorError, runProcessorM)
import System.DirWatch.PreProcessor (runPreProcessor, yieldFilePath)

type StopCond = MVar ()
type ThreadMap = HashMap AbsPath (HashSet RunningWatcher)
type DirMap = HashMap AbsPath (HashSet RunnableWatcher)

type Failure = (POSIXTime,ProcessorError)
data RunningWatcher
  = RunningWatcher {
      rwHandle   :: SomeThreadHandle ProcessorError
    , rwWatcher  :: RunnableWatcher
    , rwFailures :: [Failure]
    , rwStart    :: POSIXTime
    }
  | FailedWatcher {
      rwWatcher     :: RunnableWatcher
    , rwFailures    :: [Failure]
    }
instance Hashable RunningWatcher where
  hashWithSalt n RunningWatcher{..} = hashWithSalt n rwWatcher
  hashWithSalt n FailedWatcher{..}  = hashWithSalt n rwWatcher + 1

instance Eq RunningWatcher where
  (==) = (==) `on` rwWatcher

watchedEvents :: [EventVariety]
watchedEvents = [MoveIn, CloseWrite]

data ChanMessage
  = Work [RunnableWatcher] AbsPath
  | WakeUp
  | Finish

data WatcherEnv
  = WatcherEnv {
      wConfig   :: RunnableConfig
    , wChan     :: Chan ChanMessage
    , wInotify  :: INotify
    , wDirMap   :: DirMap
  }


data WatcherState
  = WatcherState {
      wThreads :: ThreadMap
  }
instance Monoid WatcherState where
  mempty  = WatcherState empty
  mappend a b
   = WatcherState {
       wThreads = unionWith HS.union (wThreads a) (wThreads b)
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
mkDirMap ws = foldl' go empty $ [(w,p) | w<-ws, p<-wGlobs w]
  where
    go m (w,p) = insertWith HS.union (takePatternDirectory p) (HS.singleton w) m

endLoop :: StopCond -> IO ()
endLoop mvar = putMVar mvar ()

mkStopCond :: IO StopCond
mkStopCond = newEmptyMVar

setupWatches :: WatcherM ()
setupWatches = do
  dir_watchers <- asks (toList . wDirMap)
  chan <- asks wChan
  forM_ dir_watchers $ \(baseDir, watcherSet) -> do
    let names = intercalate ", " $ map wName watchers
        watchers = HS.toList watcherSet
        handleFile filePath = do
          let matchedWatchers = filter (any (p `globMatch`) . wGlobs) watchers
              p = joinAbsPath baseDir [filePath]
          when (not (null matchedWatchers)) $ do
            runStderrLoggingT $ $(logInfo) $ fromStrings [show p," has arrived"]
            writeChan chan $ Work matchedWatchers p
    mError <- addIWatch baseDir $ \case
                MovedIn{isDirectory=False, ..}       -> handleFile filePath
                Closed{ wasWriteable=True, isDirectory=False
                      , maybeFilePath=Just filePath} -> handleFile filePath
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
  msg <- getMessage
  case msg of
    Work ws file -> runWatchersOnFile (zip ws (repeat [])) file >> loop
    WakeUp       -> loop
    Finish       -> return ()

checkExistingFiles :: WatcherM ()
checkExistingFiles = do
  dir_watchers <- asks wDirMap
  waitSecs <- asks (cfgStableTime . wConfig)
  chan <- asks wChan
  void $ liftIO $ forkChild $ do
    existingMap <- fmap (fromListWith (++) . concat . concat . concat) $
      forM (elems dir_watchers) $ \watchers -> do
        forM (HS.toList watchers) $ \watcher -> do
          forM (wGlobs watcher) $ \globPattern ->  do
            paths <- absPathsMatching globPattern
            forM paths $ \abspath -> return (abspath, [watcher])
    existing <- forM (toList existingMap) $ \(abspath, watchers) -> do
      mt <- fmap modificationTime $ getFileStatus (toFilePath abspath)
      return (mt, watchers, abspath)
    sleep waitSecs
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
  retryInterval <- asks (cfgRetryInterval . wConfig)
  (mRemaining,retries) <- fmap unzip $ forM running $ \(fname, runningPs) -> do
    eRemaining <- forM (HS.toList runningPs) $ \fp -> do
      now <- liftIO getPOSIXTime
      let goNext t = (now-t) > retryInterval
      case fp of
        RunningWatcher{..} -> fmap Right $ do
          result <- liftIO (tryWaitSomeChild rwHandle)
          case result of
            Nothing         -> return (Just fp)
            Just Nothing    -> return Nothing
            Just (Just err) -> do
              let errParts = [ "Watcher ", show (wName rwWatcher)
                             , " failed on ", show fname, " "
                             , retryText (length rwFailures + 1), ": "
                             , show err]
              numRetries <- asks (cfgNumRetries . wConfig)
              if length rwFailures < numRetries
                then do
                  $(logError) $ fromStrings $ errParts ++ [", will retry later"]
                  return $ Just $
                    FailedWatcher { rwWatcher=rwWatcher
                                  , rwFailures=(now,err):rwFailures}
                else do
                  $(logError) $ fromStrings $ errParts ++ [
                    ", which is too much. Will NOT retry anymore"]
                  return Nothing
        FailedWatcher{rwFailures=fails@((tFail,_):_), ..} | goNext tFail ->
          return $ Left $ (rwWatcher,fails)
        _ -> return $ Right $ Just fp
    case second catMaybes (partitionEithers eRemaining) of
      ([], []) -> do
        $(logInfo) $ fromStrings ["Finished processing ", show fname]
        archiveFile fname
        return (Nothing, return())
      (retries, []) ->
        return (Nothing, runWatchersOnFile retries fname)
      (retries, remaining) ->
        return ( Just (fname, HS.fromList remaining)
               , runWatchersOnFile retries fname)
  put state {wThreads=HM.fromList (catMaybes mRemaining)}
  sequence_ retries
  when (not (null retries)) $ do
    chan <- asks wChan
    void . liftIO . forkIO $ (sleep retryInterval >> writeChan chan WakeUp)

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . floor . (*1000000)

    


retryText :: Int -> String
retryText num
  | num == 0  = ""
  | otherwise = concat [prefix, show num, numSuffix, suffix]
  where numSuffix
          | num `mod` 10 == 1 = "st"
          | num `mod` 10 == 2 = "nd"
          | num `mod` 10 == 3 = "rd"
          | otherwise         = "th"
        prefix = "for the "
        suffix = " time"

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
          $(logWarn) $ fromStrings [ "Could not archive ", show fname, ": "
                                   , show e]
        Right () ->
          $(logInfo) $ fromStrings ["Archived ", show fname, " -> ", finalDest]
    Nothing -> return ()



              
runWatchersOnFile :: [(RunnableWatcher,[Failure])] -> AbsPath -> WatcherM ()
runWatchersOnFile watchers filename = do
  cfg <- processorConfig
  chan <- asks wChan
  start <- liftIO getPOSIXTime
  fps <- forM watchers $ \(wch,failures) -> do
    let proc = processWatcher (posixSecondsToUTCTime start) filename wch
    liftIO $ do
      th <- forkChild $ finally (runProcessorM cfg proc) (writeChan chan WakeUp)
      return RunningWatcher { rwHandle   = toSomeThreadHandle th
                            , rwStart    = start
                            , rwWatcher  = wch
                            , rwFailures = failures}
  addToRunning filename fps
  where

processWatcher :: UTCTime -> AbsPath -> RunnableWatcher -> ProcessorM ()
processWatcher now abspath Watcher{..} = do
  $(logDebug) $ fromStrings ["Running \"", wName, "\" on ", show abspath]
  case wProcessor of
    Just processor  -> do
      let pathPreproc  = join $ fmap wpPreprocessor $
                           find (globMatch abspath . wpGlob) wPaths
          preprocessor = fromMaybe yieldFilePath pathPreproc
          preprocess   = runPreProcessor source now (preprocessor filepath)
          process      = uncurry processor
          source       = sourceFile filepath
          filepath     = toFilePath abspath
      preprocess >>= mapM_ process
      $(logDebug) $ fromStrings ["Finished \"", wName, "\" on ", show abspath]
    Nothing -> $(logInfo) $ fromStrings [wName, " has no processor"]


wGlobs :: Watcher a b -> [AbsPath]
wGlobs = map wpGlob . wPaths

processorConfig :: WatcherM ProcessorConfig
processorConfig = do
  env <- asks (cfgShellEnv . wConfig)
  return $ def {pShellEnv = env}


addToRunning
  :: AbsPath
  -> [RunningWatcher]
  -> WatcherM ()
addToRunning path fps = do
  s@WatcherState{wThreads=ts} <- get
  put $ s {wThreads = insertWith HS.union path (HS.fromList fps) ts}
