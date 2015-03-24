{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch (
    Config (..)
  , RunnableConfig
  , RunnableWatcher
  , Watcher (..)
  , WatchedPath (..)
  , AbsPath
  , mkAbsPath
  , Options (..)
  , compileWithSymTables
  , mainDirWatcher
  , forkDirWatcher
  , symbolTable
  , def
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (when, void)
import Control.Monad.Reader (MonadReader, ReaderT(..), asks)
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_, tryPutMVar)
import Control.Exception (finally)
import Data.Aeson (FromJSON, Object)
import Data.Default (def)
import Data.Monoid (mconcat)
import Data.Typeable (Typeable, cast)
import qualified Data.HashMap.Strict as HM
import Data.Yaml (decodeFileEither)
import System.INotify (
  INotify, Event(..), EventVariety(..), withINotify, addWatch, removeWatch)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import System.DirWatch.Logging (Priority(..), setupLogging, logInfo, logError)
import System.DirWatch.Util (AbsPath, mkAbsPath)
import System.DirWatch.Config (
    RunnableConfig, SerializableConfig, RunnableWatcher, Compiler(..)
  , Config(..), Code(..) , CompilerError(..), SymbolTable(..), CompilerEnv(..)
  , Watcher(..), WatchedPath(..), symbolTable, compileConfig)
import System.DirWatch.Watcher (
  StopCond, mkStopCond, runWatchLoop, waitForJobsToComplete, endLoop)
import System.DirWatch.PreProcessor (PreProcessor)
import System.DirWatch.Processor (Processor, ProcessorM)
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
          ( long "log-level"
         <> short 'l'
         <> help "Priority for the log messages that go to stderr"
         <> value INFO)
      <*> option auto
          ( long "syslog-level"
         <> help "Priority for the log messages that go to syslog"
         <> value ERROR)


mainDirWatcher
  :: FromJSON a
  => (a -> IO (Either CompilerError RunnableConfig))
  -> [InfoMod Options]
  -> IO ()
mainDirWatcher compiler infoOpts = do
  progOpts@Options{..} <- execParser opts
  setupLogging optSyslogPriority optPriority
  runWithOptions compiler progOpts
  where opts = info (helper <*> options) (mconcat (fullDesc:infoOpts))

forkDirWatcher :: RunnableConfig -> IO (IO ())
forkDirWatcher config = do
  stopCond <- mkStopCond
  void $ forkIO $ runWatchLoop config Nothing stopCond >>= waitForJobsToComplete
  return (endLoop stopCond)


runWithOptions
  :: FromJSON a
  => (a -> IO (Either CompilerError RunnableConfig)) -> Options -> IO ()
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
  state <- case eConfig of
    Right initialConfig -> withINotify $ \ino -> do
      when optReload $ do
        watchNewPluginDirs pluginDirs initialConfig
        watchConfig optConfigFile stopCond pluginDirs ino
      mainLoop initialConfig Nothing
    Left err -> logCompileError err >> error "Unable to load config"
  waitForJobsToComplete state
  where
    decodeAndCompile = do
      ePConfig <- decodeFileEither optConfigFile
      case ePConfig of
        Right pConfig -> compiler pConfig
        Left e        -> return . Left . ParseError $ e

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
        doWatch <- readIORef doWatchConfig
        when doWatch $ do
          writeIORef doWatchConfig False
          void $ addWatch ino [MoveIn,Modify,OneShot] configFile $
            const (writeIORef doWatchConfig True >> signalReload)
  loop

logCompileError :: MonadIO m => CompilerError -> m ()
logCompileError (ParseError err)
  = $(logError) $ "Error when parsing config: " ++ show err
logCompileError (UnresolvedSymbol sym)
  = $(logError) $ "Unresolved symbol: " ++ show sym
logCompileError (CompilerError errs)
  = $(logError) $ unlines ("Could not compile config:":errs)
logCompileError (InternalCompilerError err)
  = $(logError) $ "Internal compiler error, please file a bug report: " ++ err


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

newtype StaticResolver m a
  = StaticResolver {
      unStaticResolver :: ReaderT (CompilerEnv (StaticResolver m)) m a
      }
  deriving ( Functor, Applicative, Monad, MonadThrow, MonadCatch
           , MonadReader (CompilerEnv (StaticResolver m)))

instance (Functor m, Applicative m, Monad m, MonadThrow m, MonadCatch m)
  => Compiler (StaticResolver m) where
  type CompilerBase (StaticResolver m) = m
  data CompilerConfig (StaticResolver m) = SymbolTables
         (SymbolTable (Object -> Either String Processor))
         (SymbolTable (Object -> Either String (PreProcessor ProcessorM)))
  runCompiler env = flip runReaderT env . unStaticResolver
  compileCode :: forall a. Typeable a => Code -> StaticResolver m a
  compileCode spec =
    case spec of
      EvalCode{}          -> throwCompilerError "Cannot evaluate code"
      InterpretedPlugin{} -> throwCompilerError "Cannot interpret plugins"
      LoadedPlugin{..} -> do
        SymbolTables (SymbolTable procs) (SymbolTable preprocs) <- asks ceConfig
        case cast (HM.lookup codeSymbol procs) of
          Just obj -> applyParams obj codeSymbol codeParams
          Nothing  -> case cast (HM.lookup codeSymbol preprocs) of
            Just obj -> applyParams obj codeSymbol codeParams
            Nothing  -> throwM $ InternalCompilerError "Invalid result type"
    where 
      applyParams
        :: Maybe (Object -> Either String a) -> String -> Object
        -> StaticResolver m a
      applyParams (Just obj) sym params =
        case obj params of
          Right v -> return v
          Left e  -> throwCompilerError $
            concat ["Could not parse parameters for ", show sym, ": ", show e]
      applyParams Nothing sym _ = throwM $ UnresolvedSymbol sym

      throwCompilerError = throwM . CompilerError . (:[])


compileWithSymTables
  :: Compiler (StaticResolver m)
  => SymbolTable (Object -> Either String Processor)
  -> SymbolTable (Object -> Either String (PreProcessor ProcessorM))
  -> SerializableConfig
  -> m (Either CompilerError RunnableConfig)
compileWithSymTables processors preprocessors = compileConfig cc
  where cc = SymbolTables processors preprocessors

