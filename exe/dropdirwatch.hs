{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Monad (when, void)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text as T (Text, pack, unlines, lines)
import System.DirWatch
import System.Environment (getArgs)
import System.Exit(exitFailure)
import System.INotify

main :: IO ()
main = do
  args <- getArgs
  let configFile
        = case args of
            [fname] -> fname
            []      -> "/etc/dropdirwatch.yaml"
            _       -> error $ "Invalid args: " ++ show args
  stopCond <- mkStopCond
  pluginDirs <- newEmptyMVar
  let mainLoop initialConfig initialState = do
        newState <- liftIO $
                      runWatchLoop initialConfig initialState stopCond
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

  eConfig <- decodeAndCompile configFile
  case eConfig of
    Right initialConfig -> withINotify $ \ino -> do
      watchNewPluginDirs pluginDirs initialConfig
      watchConfig configFile stopCond pluginDirs ino
      runStderrLoggingT $ mainLoop initialConfig Nothing
    Left err -> runStderrLoggingT (logCompileError err) >> exitFailure

watchNewPluginDirs
  :: MonadIO m => MVar [FilePath] -> Config w -> m ()
watchConfig
  :: FilePath -> StopCond -> MVar [FilePath] -> INotify -> IO ()

#if RELOAD_CONFIG
watchNewPluginDirs pluginDirs = liftIO . putMVar pluginDirs . cfgPluginDirs
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
#else
watchNewPluginDirs _ _ = return ()
watchConfig _ _ _ _    = return ()
#endif

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = cond >>= flip when act

logCompileError :: MonadLogger m => [Text] -> m ()
logCompileError = $(logError) . ("\n"<>) . T.unlines

decodeAndCompile :: FilePath -> IO (Either [Text] RunnableConfig)
decodeAndCompile fname = do
  ePConfig <- decodeFileEither fname
  case ePConfig of
    Right pConfig -> do
      eRet <- compileConfig pConfig
      case eRet of
        Right c -> return (Right c)
        Left es -> return . Left $
          ["Error when compiling config:\n"] ++ T.lines (T.pack (show es))
    Left e        -> return . Left $
                       [fromStrings ["Could not parse YAML: ", show e]]
