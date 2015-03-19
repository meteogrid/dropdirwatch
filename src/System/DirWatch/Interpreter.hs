{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Interpreter (
    CompilerConfig (..)
  , runCompiler
  , compileWatcher
  , compileConfig
) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter (
    InterpreterT
  , InterpreterError
  , runInterpreter
  , interpret
  , as
  , setImportsQ
  , loadModules
  , setTopLevelModules
  , set
  , searchPath
  , OptionVal ((:=))
  )
import System.DirWatch.Config (
    Config(..)
  , Watcher(..)
  , ModuleImport (..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableConfig
  , RunnableWatcher
  , Code (..)
  , ProcessorCode (..)
  , compileWith
  )
import System.DirWatch.Processor (Processor, shellProcessor)

data CompilerConfig
  = CompilerConfig {
      ccSearchPath :: [FilePath]
    , ccImports    :: [ModuleImport]
  }

newtype Compiler a
  = Compiler {
      unCompiler :: InterpreterT (ReaderT CompilerConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig -> IO (Either InterpreterError RunnableConfig)
compileConfig c = runCompiler cConfig $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}
  where
    cConfig = CompilerConfig {
        ccSearchPath = cfgPluginDirs c
      , ccImports    = cfgImports c
      }

runCompiler :: CompilerConfig -> Compiler a -> IO (Either InterpreterError a)
runCompiler c = flip runReaderT c . runInterpreter . unCompiler


compileWatcher :: SerializableWatcher -> Compiler RunnableWatcher
compileWatcher w = do
  mPp <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $ compileWith compileCode p
  mP <- case wProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $ compileWith compileProcessor p
  return $ w {wPreProcessor=mPp, wProcessor=mP}

compileProcessor :: ProcessorCode -> Compiler Processor
compileProcessor (ProcessorCode code)  = compileCode code
compileProcessor (ProcessorShell cmds) = return (shellProcessor cmds)

compileCode :: forall a. Typeable a => Code -> Compiler a
compileCode spec = Compiler $ do
  sp <- lift $ asks ccSearchPath
  globalImports <- lift $ asks ccImports
  set [searchPath := sp]
  case spec of
    EvalCode{..} -> do
      setModuleImports $ concat [pluginImports, globalImports, codeImports]
      interpret codeEval as
    ImportCode{..} -> do
      loadModules [codeModule]
      setTopLevelModules [codeModule]
      setModuleImports $ concat [pluginImports, globalImports]
      let cmd = concat [ "\\c -> case parseEither parseJSON (Object c) of {"
                       , "          Right v -> Right (", codeSymbol, " v); "
                       , "          Left e  -> Left e;                     "
                       , "          }                                      "
                       ]
      eO <- interpret cmd as
      case eO codeParams of 
        Right o -> return o
        Left e -> fail $ concat ["Error when compiling ", codeModule, ":"
                                , codeSymbol, ": ", e]

setModuleImports
  :: [ModuleImport] -> InterpreterT (ReaderT CompilerConfig IO) ()
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
