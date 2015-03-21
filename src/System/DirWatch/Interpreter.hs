{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Interpreter (
    runCompiler
  , compileWatcher
  , compileConfig
  , InterpreterError (..)
  , GhcError (..)
) where

import Control.Monad (forM)
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (lift)
import qualified Data.HashMap.Strict as HM
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter (
    InterpreterT
  , InterpreterError (..)
  , GhcError (..)
  , interpret
  , as
  , setImportsQ
  , loadModules
  , setTopLevelModules
  , set
  , searchPath
  , OptionVal ((:=))
  )
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.DirWatch.Config (
    Config(..)
  , Watcher(..)
  , WatchedPath (..)
  , SymOrCode (..)
  , SymbolTable (..)
  , ModuleImport (..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableConfig
  , RunnableWatcher
  , Code (..)
  , ProcessorCode (..)
  )
import System.DirWatch.Processor (Processor, shellProcessor)
import System.FilePath.Glob (namesMatching)

newtype Compiler a
  = Compiler {
      unCompiler :: InterpreterT (ReaderT SerializableConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig
  -> IO (Either InterpreterError (RunnableConfig Code ProcessorCode))
compileConfig c = runCompiler c $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers = watchers}

runCompiler
  :: SerializableConfig -> Compiler a -> IO (Either InterpreterError a)
runCompiler c act = do
  pkgDbDirs <- fmap concat $ mapM namesMatching (cfgPackageDbs c)
  let args = ["-package-db="++d | d <-pkgDbDirs]
  flip runReaderT c . unsafeRunInterpreterWithArgs args $ unCompiler act


compileWatcher :: SerializableWatcher -> Compiler RunnableWatcher
compileWatcher w = do
  paths <- forM (wPaths w) $ \wp@WatchedPath{..} -> do
             pp <- case wpPreprocessor of
                     Nothing -> return Nothing
                     Just pp -> fmap Just $
                       compileSymOrCodeWith cfgPreProcessors compileCode pp
             return wp {wpPreprocessor=pp}
  mP <- case wProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $
            compileSymOrCodeWith cfgProcessors compileProcessor p
  return $ w {wPaths=paths, wProcessor=mP}

compileSymOrCodeWith
  :: (SerializableConfig -> SymbolTable t)
  -> (t -> Compiler b) -> SymOrCode t -> Compiler b
compileSymOrCodeWith _ func (SymCode code)         = func code
compileSymOrCodeWith symGetter func (SymName name) = do
  SymbolTable syms <- Compiler $ lift $ asks symGetter
  case HM.lookup name syms of
      Nothing   -> fail ("Unresolved symbol: " ++ name)
      Just code -> func code


compileProcessor :: ProcessorCode -> Compiler Processor
compileProcessor (ProcessorCode code)  = compileCode code
compileProcessor (ProcessorShell cmds) = return (shellProcessor cmds)

compileCode :: forall a. Typeable a => Code -> Compiler a
compileCode spec = Compiler $ do
  sp <- lift $ asks cfgPluginDirs
  globalImports <- lift $ asks cfgImports
  set [searchPath := sp]
  case spec of
    EvalCode{..} -> do
      setModuleImports $ concat [pluginImports, globalImports, codeImports]
      interpret codeEval as
    InterpretedPlugin{..} -> do
      loadModules [codeModule]
      setTopLevelModules [codeModule]
      ePlugin <- compilePlugin codeSymbol codeParams
      case ePlugin of 
        Right o -> return o
        Left e -> fail $ concat ["Error when compiling ", codeModule, ":"
                                , codeSymbol, ": ", e]
    LoadedPlugin{..} -> do
      ePlugin <- compilePlugin codeSymbol codeParams
      case ePlugin of 
        Right o -> return o
        Left e -> fail $ concat ["Error when compiling ", codeSymbol, ": ", e]

compilePlugin
  :: (Typeable t, Typeable b)
  => String -> t -> InterpreterT (ReaderT SerializableConfig IO) b
compilePlugin symbol params = do
  globalImports <- lift $ asks cfgImports
  setModuleImports $ concat [pluginImports, globalImports]
  let cmd = concat [ "\\c -> case parseEither parseJSON (Object c) of {"
                   , "          Right v -> Right (", symbol, " v);     "
                   , "          Left e  -> Left e;                     "
                   , "          }                                      "
                   ]
  ePartialPlugin <- interpret cmd as
  return (ePartialPlugin params)


setModuleImports
  :: [ModuleImport] -> InterpreterT (ReaderT SerializableConfig IO) ()
setModuleImports = setImportsQ . map unModuleImport


pluginImports :: [ModuleImport]
pluginImports = [
    ModuleImport ("Data.Aeson", Nothing)
  , ModuleImport ("Data.Aeson.Types", Nothing)
  , ModuleImport ("System.DirWatch.PluginAPI", Nothing)
  , ModuleImport ("Data.HashMap.Strict", Nothing)
  , ModuleImport ("Data.Text", Nothing)
  , ModuleImport ("Data.ByteString", Nothing)
  , ModuleImport ("Control.Monad.Trans.Resource", Nothing)
  , ModuleImport ("Data.Conduit", Nothing)
  , ModuleImport ("Prelude", Nothing)
  ]
