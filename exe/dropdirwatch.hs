{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
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
  let mainLoop initialConfig initialState = do
        newState <- liftIO $ runWatchLoop initialConfig initialState stopCond
        $(logInfo) $ fromStrings ["Reloading ", configFile]
        eNewConfig <- liftIO $ decodeAndCompile configFile
        case eNewConfig of
          Right newConfig -> mainLoop newConfig (Just newState)
          Left e -> do 
            logCompileError e
            mainLoop initialConfig (Just newState)
      watchConfig ino
        = void $ addWatch ino [MoveIn,Modify,OneShot] configFile $ const $ do
            endLoop stopCond
            watchConfig ino
  eConfig <- decodeAndCompile configFile
  case eConfig of
    Right initialConfig -> withINotify $ \ino -> do
      watchConfig ino
      runStderrLoggingT $ mainLoop initialConfig Nothing
    Left err -> runStderrLoggingT (logCompileError err) >> exitFailure


logCompileError :: MonadLogger m => [Text] -> m ()
logCompileError e = mapM_ $(logError) e

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
