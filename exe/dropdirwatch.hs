{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (when, void)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
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
  oldWatches <- newIORef []
  doWatchConfig <- newIORef True
  let mainLoop initialConfig initialState = do
        newState <- liftIO $
                      runWatchLoop initialConfig initialState stopCond
        eNewConfig <- liftIO $ decodeAndCompile configFile
        case eNewConfig of
          Right newConfig -> do
            liftIO $ putMVar pluginDirs (cfgPluginDirs newConfig)
            mainLoop newConfig (Just newState)
          Left e -> do 
            logCompileError e
            $(logInfo) "Will continue with last valid version"
            liftIO $ putMVar pluginDirs (cfgPluginDirs initialConfig)
            mainLoop initialConfig (Just newState)
      watchConfig ino = do
        let watchDir d = addWatch ino [CloseWrite] d handleEv
            handleEv Closed{ isDirectory   = False
                           , maybeFilePath = Just f
                           , wasWriteable  = True}
              | take 2 (reverse f) == reverse "hs" = signalReload
            handleEv _ = return ()
            removeWatches = readIORef oldWatches >>= mapM_ removeWatch
            signalReload = endLoop stopCond >> watchConfig ino
        removeWatches
        takeMVar pluginDirs >>= mapM watchDir >>= writeIORef oldWatches
        whenM (readIORef doWatchConfig) $ do
          writeIORef doWatchConfig False
          void $ addWatch ino [MoveIn,Modify,OneShot] configFile $
            const (writeIORef doWatchConfig True >> signalReload)

  eConfig <- decodeAndCompile configFile
  case eConfig of
    Right initialConfig -> withINotify $ \ino -> do
      putMVar pluginDirs (cfgPluginDirs initialConfig)
      watchConfig ino
      runStderrLoggingT $ mainLoop initialConfig Nothing
    Left err -> runStderrLoggingT (logCompileError err) >> exitFailure

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = cond >>= flip when act

logCompileError :: MonadLogger m => [Text] -> m ()
logCompileError e = mapM_ ($(logError) . ("\n"<>)) e

decodeAndCompile :: FilePath -> IO (Either [Text] RunnableConfig)
decodeAndCompile fname = do
  ePConfig <- decodeFileEither fname
  case ePConfig of
    Right pConfig -> do
      eRet <- compileConfig pConfig
      case eRet of
        Right c -> return (Right c)
        Left es -> return (Left ("Error when compiling config:":es))
    Left e        -> return . Left $
                       [fromStrings ["Could not parse YAML: ", show e]]
