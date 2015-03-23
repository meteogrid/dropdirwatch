{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module System.DirWatch.Interpreter (
    runCompiler
  , compileWatcher
  , compileConfig
  , InterpreterError (..)
  , GhcError (..)
) where

import Control.Monad (forM)
import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (lift)
import qualified Data.HashMap.Strict as HM
import Data.Typeable (Typeable)
import Data.Aeson (Object)
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

instance (MonadReader SerializableConfig) Compiler where
  ask = Compiler (lift  ask)

compileConfig = compileWith runCompiler

compileWith
  :: (SerializableConfig -> Compiler (RunnableConfig Code ProcessorCode)
      -> IO (Either [String] (RunnableConfig Code ProcessorCode)))
  -> SerializableConfig
  -> IO (Either [String] (RunnableConfig Code ProcessorCode))
compileWith f c = f c $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers = watchers}

toLines :: InterpreterError -> [String]
toLines err
  = (prefix:) $ case err of
      WontCompile es -> concat (map (lines . errMsg) es)
      es             -> lines (show es)
  where prefix = "Error when compiling config:"

runCompiler
  :: SerializableConfig -> Compiler a -> IO (Either [String] a)
runCompiler c act = do
  pkgDbDirs <- fmap concat $ mapM namesMatching (cfgPackageDbs c)
  let args = ["-package-db="++d | d <-pkgDbDirs]
  fmap (either (Left . toLines) Right)
    . flip runReaderT c
    . unsafeRunInterpreterWithArgs args
    $ unCompiler act


compileWatcher w = do
  paths <- mapM compilePath (wPaths w)
  mP <- case wProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $ do
            syms <- asks cfgProcessors
            compileSymOrCodeWith syms compileProcessor p
  return $ w {wPaths=paths, wProcessor=mP}

compilePath wp = do
  pp' <- case wpPreprocessor wp of
    Just pp ->
      fmap Just $ do
        syms <- asks cfgPreProcessors
        compileSymOrCodeWith syms compileCode pp
    Nothing -> return Nothing
  return $ wp {wpPreprocessor=pp'}

compileSymOrCodeWith _ func (SymCode code)         = func code
compileSymOrCodeWith (SymbolTable syms) func (SymName name) = do
  case HM.lookup name syms of
      Nothing   -> fail ("Unresolved symbol: " ++ name)
      Just code -> func code


compileProcessor (ProcessorCode code)  = compileCode code
compileProcessor (ProcessorShell cmds) = return (shellProcessor cmds)

class MonadReader SerializableConfig m => CompilesCode m where
  compileCode :: Typeable a => Code -> m a

instance CompilesCode Compiler where
  compileCode spec = Compiler $ do
    sp <- lift $ asks cfgPluginDirs
    set [searchPath := sp]
    case spec of
      EvalCode{..} -> do
        globalImports <- lift $ asks cfgImports
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

compilePlugin symbol params = do
  globalImports <- lift $ asks cfgImports
  setModuleImports $ concat [pluginImports, globalImports]
  let cmd = "mkPlugin " ++ symbol
  ePartialPlugin <- interpret cmd as
  return (ePartialPlugin params)


setModuleImports = setImportsQ . map unModuleImport


pluginImports :: [ModuleImport]
pluginImports = [
    ModuleImport ("System.DirWatch.PluginAPI", Nothing)
  , ModuleImport ("Data.HashMap.Strict", Nothing)
  , ModuleImport ("Data.Text", Nothing)
  , ModuleImport ("Data.ByteString", Nothing)
  , ModuleImport ("Control.Monad.Trans.Resource", Nothing)
  , ModuleImport ("Data.Conduit", Nothing)
  , ModuleImport ("Prelude", Nothing)
  ]
