{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch.Watcher where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Exception (SomeException, try)
import Control.Concurrent.MVar (
    MVar
  , takeMVar
  )
import Control.Monad (forM_, forM, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, asks, ReaderT, runReaderT)
import Control.Monad.State.Strict (
    MonadState (get,put)
  , StateT
  , runStateT
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Default (def)
import Data.Monoid (Monoid(..))
import qualified Data.ByteString.Lazy as LBS
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
import Data.Time.Clock (getCurrentTime, utctDay)
import System.INotify (
    INotify
  , Event (..)
  , EventVariety (..)
  , withINotify
  , addWatch
  )
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath.GlobPattern ((~~))
import System.FilePath.Posix (joinPath, takeDirectory, normalise)
import System.IO (IOMode(ReadMode))
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
  , killChild
  )
import System.DirWatch.Logging (
    LoggingT
  , MonadLogger
  , runStderrLoggingT
  , logDebug
  , logInfo
  , logError
  , fromStrings
  )
import System.DirWatch.Processor (
    ProcessorConfig (..)
  , ProcessorError
  , runProcessorM
  , withFile
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

data WatcherEnv
  = WatcherEnv {
      wConfig   :: RunnableConfig
    , wStopCond :: StopCond
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
           , MonadState WatcherState, MonadIO)

        
runWatcherEnv
  :: WatcherEnv -> WatcherState -> WatcherM a -> IO (a, WatcherState)
runWatcherEnv env state
  = flip runStateT state
  . flip runReaderT env
  . runStderrLoggingT
  . unWatcherM

runWatcherM
  :: RunnableConfig -> Maybe WatcherState -> StopCond -> WatcherM a
  -> IO (a,WatcherState)
runWatcherM cfg mState stopCond loopFunc = withINotify $ \ino -> do
  env <- WatcherEnv <$> pure cfg <*> pure stopCond <*> newChan <*> pure ino
  let state = fromMaybe mempty mState
  loopth <- forkChild $ runWatcherEnv env state loopFunc
  wakerth  <- forkChild  (waker (wChan env))
  takeMVar stopCond
  writeChan (wChan env) Finish
  ret <- waitChild loopth
  killChild wakerth
  return ret

setupWatches :: WatcherM ()
setupWatches = do
  watchers <- asks (cfgWatchers . wConfig)
  chan <- asks wChan
  forM_ watchers $ \watcher -> do
    forM_ (wPaths watcher) $ \globPattern -> do
      let baseDir  = takePatternDirectory globPattern
          absPath p = joinPath [baseDir, p]
      addIWatch baseDir $ \event -> do
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

loop :: WatcherM ()
loop = do
  handleFinishedFiles
  msg <- getMessage
  case msg of
    Work wch file -> runWatcherOnFile wch file >> loop
    WakeUp        -> loop
    Finish        -> return ()

addIWatch :: FilePath -> (Event -> IO ()) -> WatcherM ()
addIWatch dir wch = do
  $(logDebug) $ fromStrings ["Watching ", dir]
  ino <- asks wInotify
  void $ liftIO $ addWatch ino watchedEvents dir wch


getMessage :: WatcherM ChanMessage
getMessage = asks wChan >>= liftIO . readChan

waker :: Chan ChanMessage -> IO ()
waker chan = do
  threadDelay 1000000
  writeChan chan WakeUp
  waker chan

handleFinishedFiles :: WatcherM ()
handleFinishedFiles = do
  state@WatcherState{wThreads=ts} <- get
  let running = HM.toList ts
  remaining <- fmap catMaybes $ forM running $ \(fname, runningPs) -> do
    remainingPs <- fmap catMaybes $ forM runningPs $ \fp -> do
      result <- liftIO (tryWaitSomeChild (fpHandle fp))
      case result of
        Nothing      -> return Nothing
        Just Nothing -> return (Just fp)
        Just (Just err) ->
          registerFailure fname (fpWatcher fp) err >> return Nothing
    if null remainingPs
      then archiveFile fname >> return Nothing
      else return (Just (fname, remainingPs))
  put state {wThreads=HM.fromList remaining}

archiveFile :: FilePath -> WatcherM ()
archiveFile fname = do
  mArchiveDir <- asks (cfgArchiveDir . wConfig)
  case mArchiveDir of
    Just archiveDir -> do
      result <- liftIO . try $ do
        time <- getCurrentTime
        let dest    = archiveDestination archiveDir (utctDay time) fname
            destDir = takeDirectory dest
        createDirectoryIfMissing True destDir
        renameFile fname dest
      case result of
        Left (e :: SomeException) -> do
          $(logError) $ fromStrings ["Could not archive ", fname, ": ", show e]
          return ()
        Right () -> return ()
    Nothing -> return ()

registerFailure :: FilePath -> RunnableWatcher -> ProcessorError -> WatcherM ()
registerFailure fname wch err = do
  $(logError) $ fromStrings ["action on ", fname, " failed: ", show err]
  return () -- TODO

              
runWatcherOnFile :: RunnableWatcher -> FilePath -> WatcherM ()
runWatcherOnFile wch@Watcher{..} filename = do
  cfg <- processorConfig
  fp <- liftIO $ do
    th <- forkChild (runProcessorM cfg action)
    start <- getPOSIXTime
    return FileProcessor { fpHandle  = toSomeThreadHandle th
                         , fpStart   = start
                         , fpWatcher = wch}
  addToRunningProcessors filename fp
  where
    action = do
      $(logInfo) $ fromStrings ["Running ", wName, " on ", filename]
      withFile filename ReadMode $ \file -> do
        content <- liftIO $ LBS.hGetContents file
        let pairs = case wPreProcessor of
              Nothing -> [(filename, content)]
              Just pp ->  (getCompiled pp) filename content
        case wProcessor of
          Just p  -> (getCompiled p) pairs
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
