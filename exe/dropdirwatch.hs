{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Environment (getArgs)
import System.DirWatch
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
            $(logError) $ fromStrings ["Could not reload config: ", show e]
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
    Left err -> error $ "Could not parse config: " ++ show err


decodeAndCompile :: FilePath -> IO (Either String RunnableConfig)
decodeAndCompile fname = do
  ePConfig <- decodeFileEither fname
  case ePConfig of
    Right pConfig -> do
      eConfig <- compileConfig pConfig
      return $ case eConfig of
        Right config -> Right config
        Left e       -> Left $ "Could not compile config: " ++  show e
    Left e    -> return . Left $ "Could not parse YAML: " ++ show e
