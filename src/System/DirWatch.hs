{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module System.DirWatch (
    Config (..)
  , Watcher (..)
  , RunnableWatcher
  , RunnableConfig
  , runWithConfigFile
  , runWithConfig
  , def
) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Default (def)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, pack, unlines, lines)
import Data.Yaml (decodeFileEither)
import System.INotify

import System.DirWatch.Interpreter
import System.DirWatch.Logging
import System.DirWatch.Config
import System.DirWatch.Watcher

runWithConfigFile :: FilePath -> Bool -> IO ()
runWithConfigFile configFile reloadConfig = do
  stopCond <- mkStopCond
  pluginDirs <- newEmptyMVar
  let mainLoop initialConfig initialState = do
        newState <- liftIO $ runWatchLoop initialConfig initialState stopCond
        if reloadConfig then do
          eNewConfig <- liftIO $ decodeAndCompile configFile
          case eNewConfig of
            Right newConfig -> do
              watchNewPluginDirs pluginDirs newConfig
              mainLoop newConfig (Just newState)
            Left e -> do 
              logCompileError e
              $(logInfo) "Will continue with last valid version"
              watchNewPluginDirs pluginDirs initialConfig
              mainLoop initialConfig (Just newState)
        else return newState

  eConfig <- decodeAndCompile configFile
  _ <- case eConfig of
    Right initialConfig -> withINotify $ \ino -> do
      when (reloadConfig) $ do
        watchNewPluginDirs pluginDirs initialConfig
        watchConfig configFile stopCond pluginDirs ino
      runStderrLoggingT $ mainLoop initialConfig Nothing
    Left err -> do
      runStderrLoggingT (logCompileError err)
      error "Unable to load config"
  -- TODO: Cleanup state
  return ()

runWithConfig :: RunnableConfig -> IO ()
runWithConfig config = do
  _ <- mkStopCond >>= runWatchLoop config Nothing
  return ()
  -- TODO: Cleanup state

watchNewPluginDirs
  :: MonadIO m => MVar [FilePath] -> Config pp p ppc pc -> m ()
watchNewPluginDirs pluginDirs = liftIO . putMVar pluginDirs . cfgPluginDirs

watchConfig
  :: FilePath -> StopCond -> MVar [FilePath] -> INotify -> IO ()
watchConfig configFile stopCond pluginDirs ino = do
  oldWatches <- newIORef []
  doWatchConfig <- newIORef True
  let watchDir d = addWatch ino [CloseWrite] d handleEv
      handleEv Closed{ isDirectory   = False
                     , maybeFilePath = Just f
                     , wasWriteable  = True}
        | take 2 (reverse f) == reverse "hs" = signalReload
      handleEv _ = return ()
      removeWatches = readIORef oldWatches >>= mapM_ removeWatch
      signalReload = endLoop stopCond >> loop
      loop = do
        removeWatches
        takeMVar pluginDirs >>= mapM watchDir >>= writeIORef oldWatches
        whenM (readIORef doWatchConfig) $ do
          writeIORef doWatchConfig False
          void $ addWatch ino [MoveIn,Modify,OneShot] configFile $
            const (writeIORef doWatchConfig True >> signalReload)
  loop

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = cond >>= flip when act

logCompileError :: MonadLogger m => [T.Text] -> m ()
logCompileError = $(logError) . ("\n"<>) . T.unlines

decodeAndCompile :: FilePath -> IO (Either [T.Text] RunnableConfig)
decodeAndCompile fname = do
  ePConfig <- decodeFileEither fname
  case ePConfig of
    Right pConfig -> do
      eRet <- compileConfig pConfig
      case eRet of
        Right c -> return (Right c)
        Left (WontCompile es) -> return . Left . map T.pack $
          "Error when compiling config:":(concat (map (lines . errMsg) es))
        Left es -> return . Left $
          ["Error when compiling config:"] ++ T.lines (T.pack (show es))
    Left e -> return . Left $ [fromStrings ["Could not parse YAML: ", show e]]
