{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.DirWatch (
    Config (..)
  , Options (..)
  , Watcher (..)
  , RunnableWatcher
  , RunnableConfig
  , runWithOptions
  , compileWith
  , mainWithCompiler
  , symbolTable
  , setupLogging
  , def
) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, void)
import Control.Monad.Trans.Except (runExcept, throwE)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, tryPutMVar)
import Control.Exception (finally)
import Data.Aeson (FromJSON, Object)
import Data.Default (def)
import Data.Monoid (mconcat)
import qualified Data.HashMap.Strict as HM
import Data.Yaml (decodeFileEither)
import System.INotify
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.DirWatch.Logging (Priority(..), setupLogging, logInfo, logError)
import System.DirWatch.Config
import System.DirWatch.Watcher
import System.DirWatch.PreProcessor (PreProcessor)
import System.DirWatch.Processor (Processor, ProcessorM, shellProcessor)
import Options.Applicative

data Options
   = Options {
       optConfigFile     :: FilePath
     , optReload         :: Bool
     , optPriority       :: Priority
     , optSyslogPriority :: Priority
   }

options :: Parser Options
options = Options
      <$> strOption
          ( long "config"
         <> short 'c'
         <> metavar "CONFIG"
         <> help "Path to the configuration YAML file")
      <*> switch
          ( long "reload"
         <> short 'r'
         <> help "Wheter to reload the configuration/plugins when modified")
      <*> option auto
          ( long "--log-level"
         <> short 'l'
         <> help "Priority for the log messages that go to stderr"
         <> value INFO)
      <*> option auto
          ( long "--syslog-level"
         <> help "Priority for the log messages that go to syslog"
         <> value ERROR)


mainWithCompiler
  :: FromJSON a
  => (a -> IO (Either [String] (RunnableConfig ppc pc)))
  -> [InfoMod Options]
  -> IO ()
mainWithCompiler compiler infoOpts = do
  progOpts@Options{..} <- execParser opts
  setupLogging optSyslogPriority optPriority
  runWithOptions compiler progOpts
  where opts = info (helper <*> options) (mconcat (fullDesc:infoOpts))


runWithOptions
  :: FromJSON a
  => (a -> IO (Either [String] (RunnableConfig ppc pc))) -> Options -> IO ()
runWithOptions compiler Options{..} = do
  stopCond <- mkStopCond
  pluginDirs <- newEmptyMVar
  interrupted <- newMVar 0
  let mainLoop initialConfig initialState = do
        newState <- liftIO $ runWatchLoop initialConfig initialState stopCond
        shouldStop <- liftIO $ wasInterrupted interrupted
        if optReload && not shouldStop then do
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
      when optReload $ do
        watchNewPluginDirs pluginDirs initialConfig
        watchConfig optConfigFile stopCond pluginDirs ino
      mainLoop initialConfig Nothing
    Left err -> logCompileError err >> error "Unable to load config"
  -- TODO: Cleanup state
  return ()
  where
    decodeAndCompile = do
      ePConfig <- decodeFileEither optConfigFile
      case ePConfig of
        Right pConfig -> compiler pConfig
        Left e -> return . Left $ [concat ["Could not parse YAML: ", show e]]

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

logCompileError :: MonadIO m => [String] -> m ()
logCompileError = $(logError) . ("\n"<>) . unlines


installSignalHandlers :: StopCond -> MVar Int -> IO ()
installSignalHandlers stopCond interrupted = do
  let handler signal =  do
        $(logInfo) $ concat ["Caught signal: ", signal]
        modifyMVar_ interrupted (return . (+1))
        void $ tryPutMVar stopCond ()
  void $ installHandler sigINT (Catch $ handler "INT") Nothing
  void $ installHandler sigTERM (Catch $ handler "TERM") Nothing

wasInterrupted :: MVar Int -> IO Bool
wasInterrupted = fmap (>0) . readMVar

compileWith
  :: Monad m
  => SymbolTable (Object -> Either String Processor)
  -> SymbolTable (Object -> Either String (PreProcessor ProcessorM))
  -> SerializableConfig
  -> (m (Either [String] (RunnableConfig Code ProcessorCode)))
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
            Just obj -> case obj codeParams of
                          Right v -> return v
                          Left e  -> throwE $ [concat [
                                        "Could not parse config for "
                                      , show codeSymbol
                                      , ": ", show e]]
            Nothing  -> throwE $ [concat [ "Plugin ", show codeSymbol
                                         , " is not defined"]]

    compileSymOrCodeWith _ func (SymCode code)         = func code
    compileSymOrCodeWith (SymbolTable syms) func (SymName name) = do
      case HM.lookup name syms of
          Nothing   -> throwE [concat ["Unresolved symbol: ", show name]]
          Just code -> func code
