{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.DirWatch (
    Config (..)
  , Watcher (..)
  , RunnableWatcher
  , RunnableConfig
  , runWithConfigFile
  , compileWith
  , mainWithCompiler
  , symbolTable
  , mkPlugin
  , def
) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, void)
import Control.Monad.Trans.Except (runExcept, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, tryPutMVar)
import Control.Exception (finally)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON(parseJSON), Value(Object))
import Data.Aeson.Types (parseEither)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (Text, unlines)
import Data.Yaml (decodeFileEither)
import System.INotify
import System.Environment (getArgs)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.DirWatch.Logging
import System.DirWatch.Config
import System.DirWatch.Watcher
import System.DirWatch.PreProcessor (PreProcessor)
import System.DirWatch.Processor (Processor, ProcessorM, shellProcessor)

mainWithCompiler
  :: FromJSON a
  => (a -> IO (Either [T.Text] (RunnableConfig ppc pc)))
  -> FilePath
  -> IO ()
mainWithCompiler compiler defaultConfigFile = do
  args <- getArgs
  let (configFile, reload) =
        case args of
          ["--reload"]        -> (defaultConfigFile, True)
          [fname]             -> (fname, False)
          [fname, "--reload"] -> (fname, True)
          []                  -> (defaultConfigFile, False)
          _                   -> error $ "Invalid args: " ++ show args
  runWithConfigFile compiler configFile reload

runWithConfigFile
  :: FromJSON a
  => (a -> IO (Either [T.Text] (RunnableConfig ppc pc)))
  -> FilePath -> Bool -> IO ()
runWithConfigFile compiler configFile reloadConfig = do
  stopCond <- mkStopCond
  pluginDirs <- newEmptyMVar
  interrupted <- newMVar 0
  let mainLoop initialConfig initialState = do
        newState <- liftIO $ runWatchLoop initialConfig initialState stopCond
        shouldStop <- liftIO $ wasInterrupted interrupted
        if reloadConfig && not shouldStop then do
          eNewConfig <- liftIO $
            finally decodeAndCompile
                    (installSignalHandlers stopCond interrupted)
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

  eConfig <- finally decodeAndCompile
                     (installSignalHandlers stopCond interrupted)
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
  where
    decodeAndCompile = do
      ePConfig <- decodeFileEither configFile
      case ePConfig of
        Right pConfig -> compiler pConfig
        Left e -> return . Left $
          [fromStrings ["Could not parse YAML: ", show e]]

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


installSignalHandlers :: StopCond -> MVar Int -> IO ()
installSignalHandlers stopCond interrupted = do
  let handler signal =  do
        runStderrLoggingT $
            $(logInfo) $ fromStrings ["Caught signal: ", signal]
        modifyMVar_ interrupted (return . (+1))
        void $ tryPutMVar stopCond ()
  void $ installHandler sigINT (Catch $ handler "INT") Nothing
  void $ installHandler sigTERM (Catch $ handler "TERM") Nothing

wasInterrupted :: MVar Int -> IO Bool
wasInterrupted = fmap (>0) . readMVar


type PolyProcessor = Value -> Either String Processor
type PolyPreProcessor = Value -> Either String (PreProcessor ProcessorM)

compileWith
  :: Monad m
  => SymbolTable PolyProcessor
  -> SymbolTable PolyPreProcessor
  -> SerializableConfig
  -> (m (Either [T.Text] (RunnableConfig Code ProcessorCode)))
compileWith processors preprocessors c@Config{..}
  = return . runExcept $ do
      watchers' <- mapM compileWatcher cfgWatchers
      return $ c {cfgWatchers=watchers'}
  where
    compileWatcher w = do
      paths <- mapM compilePath (wPaths w)
      processor <- case wProcessor w of
        Nothing -> return Nothing
        Just p  ->
          fmap Just $ compileSymOrCodeWith cfgProcessors compileProcessor p
      return $ w {wPaths=paths, wProcessor=processor}

    compilePath wp = do
      pp' <- case wpPreprocessor wp of
        Just pp ->
          fmap Just $
            compileSymOrCodeWith cfgPreProcessors (compileCode preprocessors) pp
        Nothing -> return Nothing
      return $ wp {wpPreprocessor=pp'}

    compileProcessor (ProcessorCode code)  = compileCode processors code
    compileProcessor (ProcessorShell cmds) = return $ shellProcessor cmds

    compileCode (SymbolTable syms) spec =
      case spec of
        EvalCode{}          -> throwE ["Cannot evaluate code"]
        InterpretedPlugin{} -> throwE ["Cannot interpret plugins"]
        LoadedPlugin{..} ->
          case HM.lookup codeSymbol syms of
            Just obj -> case obj (Object codeParams) of
                          Right v -> return v
                          Left e  -> throwE $ [fromStrings [
                                        "Could not parse config for "
                                      , show codeSymbol
                                      , ": ", show e]]
            Nothing  -> throwE $ [fromStrings [ "Plugin ", show codeSymbol
                                              , " is not defined"]]

    compileSymOrCodeWith _ func (SymCode code)         = func code
    compileSymOrCodeWith (SymbolTable syms) func (SymName name) = do
      case HM.lookup name syms of
          Nothing   -> throwE [fromStrings ["Unresolved symbol: ", show name]]
          Just code -> func code

mkPlugin
  :: FromJSON a
  => String -> (a -> b) -> (String, Value -> Either String b)
mkPlugin name func = (name, func')
  where
    func' config
      = case parseEither parseJSON config of
          Right v -> Right (func v)
          Left  e -> Left e
