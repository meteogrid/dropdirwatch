{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module System.DirWatch.Interpreter (interpretConfig) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch)
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
  , ConfigCompiler (..)
  , ConfigCompilerError (..)
  , CompilerEnv (..)
  , ModuleImport (..)
  , RunnableConfig
  , SerializableConfig
  , Code (..)
  , compileConfig
  )
import System.FilePath.GlobPattern (GlobPattern)
import System.FilePath.Glob (namesMatching)

newtype HintCompiler a
  = HintCompiler {
      unHintCompiler :: InterpreterT (ReaderT (CompilerEnv HintCompiler) IO) a
  } deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

instance (MonadReader (CompilerEnv HintCompiler)) HintCompiler where
  ask = HintCompiler (lift  ask)
  local f act = asks f >>= HintCompiler . liftIO . flip runCompiler act

interpretConfig
  :: SerializableConfig -> IO (Either ConfigCompilerError RunnableConfig)
interpretConfig cfg@Config{..} = compileConfig cc cfg
  where
    cc = HintCompilerConfig {
           ccGlobalImports = cfgImports
         , ccPluginDirs    = cfgPluginDirs
         , ccPackageDbs    = cfgPackageDbs
         }


instance ConfigCompiler HintCompiler where
  type CompilerMonad HintCompiler = IO
  data CompilerConfig HintCompiler
         = HintCompilerConfig {
             ccGlobalImports :: [ModuleImport]
           , ccPluginDirs    :: [FilePath]
           , ccPackageDbs    :: [GlobPattern]
           }
  runCompiler env act = do
    let packageDbs = ccPackageDbs (ceConfig env)
    pkgDbDirs <- fmap concat $ mapM namesMatching packageDbs
    let args = ["-package-db="++d | d <-pkgDbDirs]
    eResult <- flip runReaderT env
             . unsafeRunInterpreterWithArgs args
             $ unHintCompiler act
    either (throwM . ConfigCompilerError . toLines) return eResult

  compileCode spec = HintCompiler $ do
    sp <- lift $ asks (ccPluginDirs . ceConfig)
    globalImports <- lift $ asks (ccGlobalImports . ceConfig)
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
    where
      compilePlugin symbol params = do
        globalImports <- lift $ asks (ccGlobalImports . ceConfig)
        setModuleImports $ concat [pluginImports, globalImports]
        let cmd = "mkPlugin " ++ symbol
        ePartialPlugin <- interpret cmd as
        return (ePartialPlugin params)


toLines :: InterpreterError -> [String]
toLines err
  = (prefix:) $ case err of
      WontCompile es -> concat (map (lines . errMsg) es)
      es             -> lines (show es)
  where prefix = "Error when compiling config:"


setModuleImports
  :: [ModuleImport] -> InterpreterT (ReaderT (CompilerEnv HintCompiler) IO) ()
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
