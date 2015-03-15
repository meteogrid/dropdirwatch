{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Watcher where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
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
  , modify
  , runStateT
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Default (def)
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap, insertWith, empty)
import System.INotify (
    INotify
  , Event (..)
  , EventVariety (..)
  , withINotify
  , addWatch
  )
import System.FilePath.GlobPattern ((~~))
import System.FilePath.Posix (joinPath, normalise)
import System.IO (IOMode(ReadMode))
import System.DirWatch.Config (
    RunnableConfig
  , RunnableWatcher
  , Config(..)
  , Watcher(..)
  )
import System.DirWatch.Util (enumerate, patternDir)

import System.DirWatch.Threading (
    ThreadHandle
  , SomeThreadHandle
  , toSomeThreadHandle
  , forkChild
  , waitChild
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
    ProcessorM
  , ProcessorConfig (..)
  , ProcessorError
  , runProcessorM
  , withFile
  )
import qualified System.DirWatch.Processor as P

type StopCond = MVar ()
type ThreadMap = HashMap FilePath [SomeThreadHandle ProcessorError]

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

newtype WatcherM a
  = WatcherM {
      unWatcherM :: LoggingT (ReaderT WatcherEnv (StateT ThreadMap IO)) a
      }
  deriving ( Functor, Applicative, Monad, MonadLogger, MonadReader WatcherEnv
           , MonadState ThreadMap, MonadIO)

        
runWatcherEnv :: WatcherEnv -> ThreadMap -> WatcherM a -> IO (a, ThreadMap)
runWatcherEnv env tmap
  = flip runStateT tmap
  . flip runReaderT env
  . runStderrLoggingT
  . unWatcherM

runWatcherM :: RunnableConfig -> StopCond -> WatcherM a -> IO a
runWatcherM cfg stopCond loopFunc = withINotify $ \ino -> do
  env <- WatcherEnv <$> pure cfg <*> pure stopCond <*> newChan <*> pure ino
  loopth <- forkChild $ runWatcherEnv env empty loopFunc
  wakerth  <- forkChild  (waker (wChan env))
  takeMVar stopCond
  writeChan (wChan env) Finish
  (ret, tmap) <- waitChild loopth
  killChild wakerth
  -- TODO clean tmap
  return ret

setupWatches :: WatcherM ()
setupWatches = do
  watchers <- asks (cfgWatchers . wConfig)
  chan <- asks wChan
  forM_ watchers $ \watcher -> do
    forM_ (wPaths watcher) $ \globPattern -> do
      let baseDir  = patternDir globPattern
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
  cleanupProcessors
  msg <- getMessage
  case msg of
    Work wch file -> forkProcessor file (runWatcherOnFile wch file) >> loop
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

cleanupProcessors :: WatcherM ()
cleanupProcessors = return ()
                           
              
runWatcherOnFile :: RunnableWatcher -> FilePath -> ProcessorM ()
runWatcherOnFile Watcher{..} filename = do
  $(logInfo) $ fromStrings ["Running ", wName, " on ", filename]
  withFile filename ReadMode $ \file -> do
    content <- liftIO $ LBS.hGetContents file
    let pairs = case wPreProcessor of
          Nothing -> [(filename, content)]
          Just pp ->  pp filename content
    handles <- forM wProcessors $ \processor -> P.forkChild $
      mapM_ (uncurry processor) pairs
    forM_ (enumerate 1 handles) $ \(ix,th) -> do
      eResult <- P.waitChild th
      case eResult of
        Left err -> do
          $(logError) $ fromStrings [
            "Processor #", show ix, " from ", wName, " failed: ", show err]
          return ()
        Right _ -> return ()

forkProcessor :: FilePath -> ProcessorM () -> WatcherM ()
forkProcessor filename act = do
  cfg <- processorConfig
  p <- liftIO . forkChild $ (runProcessorM cfg act)
  modify (addToRunningProcessors filename p)

processorConfig :: WatcherM ProcessorConfig
processorConfig = do
  env <- asks (cfgShellEnv . wConfig)
  return $ def {pShellEnv = env}


addToRunningProcessors
  :: FilePath
  -> ThreadHandle (Either ProcessorError a)
  -> ThreadMap
  -> ThreadMap
addToRunningProcessors path th
  = insertWith (++) (normalise path) [toSomeThreadHandle th]
