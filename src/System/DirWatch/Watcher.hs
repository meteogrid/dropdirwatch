{-# LANGUAGE MultiParamTypeClasses #-}
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
) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (try, finally)
import Control.Monad (forM_, forM)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), asks, ReaderT, runReaderT)
import Control.Monad.State.Strict (
    MonadState (get,put)
  , StateT
  , runStateT
  )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Conduit (($=))
import Data.Conduit.Binary (sourceFile)
import Data.Default (def)
import Data.Monoid (Monoid(..))
import Data.Fixed (Fixed, E2)
import Data.HashMap.Strict as HM (
    HashMap
  , insertWith
  , unionWith
  , empty
  , toList
  , fromList
  )
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Clock (getCurrentTime, utctDay, utctDayTime)
import System.INotify (
    INotify
  , Event (..)
  , EventVariety (..)
  , withINotify
  , addWatch
  )
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)
import System.FilePath.GlobPattern ((~~))
import System.FilePath.Posix (joinPath, takeDirectory, normalise)
import System.IO.Error (tryIOError)
import System.DirWatch.Config (
    RunnableConfig
  , RunnableWatcher
  , Config(..)
  , Watcher(..)
  , getCompiled
  )
import System.DirWatch.Util (takePatternDirectory, archiveDestination)

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
    ProcessorConfig (..)
  , ProcessorError
  , runProcessorM
  )

type StopCond = MVar ()
type ThreadMap = HashMap FilePath [FileProcessor]

data FileProcessor
  = FileProcessor {
      fpHandle  :: SomeThreadHandle ProcessorError
    , fpWatcher :: RunnableWatcher
    , fpStart   :: POSIXTime
  }

watchedEvents :: [EventVariety]
watchedEvents = [MoveIn, CloseWrite]

data ChanMessage
  = Work RunnableWatcher FilePath
  | WakeUp
  | Finish
  deriving Show

data WatcherEnv
  = WatcherEnv {
      wConfig   :: RunnableConfig
    , wChan     :: Chan ChanMessage
    , wInotify  :: INotify
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
  env <- WatcherEnv <$> pure cfg <*> newChan <*> pure ino
  let state = fromMaybe mempty mState
  loopth <- forkChild $ runWatcherEnv env state (setupWatches >> loop)
  takeMVar stopCond
  writeChan (wChan env) Finish
  fmap snd $ waitChild loopth

endLoop :: StopCond -> IO ()
endLoop mvar = putMVar mvar ()

mkStopCond :: IO StopCond
mkStopCond = newEmptyMVar

setupWatches :: WatcherM ()
setupWatches = do
  watchers <- asks (cfgWatchers . wConfig)
  chan <- asks wChan
  forM_ watchers $ \watcher -> do
    forM_ (wPaths watcher) $ \globPattern -> do
      let baseDir  = takePatternDirectory globPattern
          absPath p = joinPath [baseDir, p]
          name = wName watcher
      mError <- addIWatch baseDir $ \event -> do
        case event of
          MovedIn{..} | absPath filePath ~~ globPattern
                      , not isDirectory ->
            writeChan chan $ Work watcher (absPath filePath)
          Closed{..} | wasWriteable, not isDirectory ->
            case maybeFilePath of
              Just filePath | absPath filePath ~~ globPattern ->
                writeChan chan $ Work watcher (absPath filePath)
              _ -> return ()
          _ -> return ()
      case mError of
        Just e ->
          $(logWarn) $ fromStrings ["Could not watch ", baseDir, " for "
                                   , name, ": ", show e]
        Nothing ->
          $(logInfo) $ fromStrings ["Watching ", baseDir, " for ", name]

loop :: WatcherM ()
loop = do
  handleFinishedFiles
  handleRetries
  msg <- getMessage
  case msg of
    Work wch file -> do
      $(logInfo) $ fromStrings ["File ", file, " arrived"]
      runWatcherOnFile wch file
      loop
    WakeUp        -> loop
    Finish        -> return ()

addIWatch :: FilePath -> (Event -> IO ()) -> WatcherM (Maybe IOError)
addIWatch dir wch = do
  ino <- asks wInotify
  result <- liftIO . try $ addWatch ino watchedEvents dir wch
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
        $(logInfo) $ fromStrings ["Finished processing ", fname]
        archiveFile fname
        return Nothing
      else return (Just (fname, remainingPs))
  put state {wThreads=HM.fromList remaining}

archiveFile :: FilePath -> WatcherM ()
archiveFile fname = do
  mArchiveDir <- asks (cfgArchiveDir . wConfig)
  case mArchiveDir of
    Just archiveDir -> do
      time <- liftIO getCurrentTime
      let dest    = archiveDestination archiveDir (utctDay time) fname
          destDir = takeDirectory dest
      exists <- liftIO $ doesFileExist dest
      let finalDest
            | exists    = dest ++ "." ++ show secs
            | otherwise = dest
          secs = realToFrac (utctDayTime time) :: Fixed E2
      $(logInfo) $ fromStrings ["Archiving ", fname, " -> ", finalDest]
      result <- liftIO . tryIOError $
        createDirectoryIfMissing True destDir >> renameFile fname finalDest
      case result of
        Left e -> do
          $(logError) $ fromStrings ["Could not archive ", fname, ": ", show e]
          return ()
        Right () -> return ()
    Nothing -> return ()

handleRetries :: WatcherM ()
handleRetries = return () -- TODO

registerFailure :: FilePath -> RunnableWatcher -> ProcessorError -> WatcherM ()
registerFailure fname wch err = do
  $(logError) $ fromStrings [wName wch, " failed on ", fname, ": ", show err]
  return () -- TODO

              
runWatcherOnFile :: RunnableWatcher -> FilePath -> WatcherM ()
runWatcherOnFile wch@Watcher{..} filename = do
  cfg <- processorConfig
  chan <- asks wChan
  fp <- liftIO $ do
    th <- forkChild $ finally (runProcessorM cfg action) (writeChan chan WakeUp)
    start <- getPOSIXTime
    return FileProcessor { fpHandle  = toSomeThreadHandle th
                         , fpStart   = start
                         , fpWatcher = wch}
  addToRunningProcessors filename fp
  where
    action = do
      $(logInfo) $ fromStrings ["Running ", wName, " on ", filename]
      case wProcessor of
        Just p  -> do
          let pairs = case wPreProcessor of
                Nothing -> [(filename, Nothing)]
                Just pp ->  (getCompiled pp) filename
          pairs' <- forM pairs $ \(fname, mConduit) -> do
            let content = case mConduit of
                            Just c  -> sourceFile filename $= c
                            Nothing -> sourceFile filename
            return (fname, content)
          (getCompiled p) pairs'
        Nothing -> return ()


processorConfig :: WatcherM ProcessorConfig
processorConfig = do
  env <- asks (cfgShellEnv . wConfig)
  return $ def {pShellEnv = env}


addToRunningProcessors
  :: FilePath
  -> FileProcessor
  -> WatcherM ()
addToRunningProcessors path fp = do
  s@WatcherState{wThreads=ts} <- get
  put $ s {wThreads = insertWith (++) (normalise path) [fp] ts}
