{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Interpreter (compileConfig) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter (
    InterpreterT
  , InterpreterError
  , ModuleName
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
  , SerializableConfig
  , SerializableWatcher
  , RunnableConfig
  , RunnableWatcher
  , Code (..)
  , ProcessorCode (..)
  )
import System.DirWatch.Processor (
    Processor
  , ShellCmd (..)
  , shellCmd
  , executeShellCmd
  )
import System.DirWatch.ShellEnv (envSet)


data CompilerConfig
  = CompilerConfig {
      ccSearchPath :: [FilePath]
  }

newtype Compiler a
  = Compiler {
      runCompiler :: InterpreterT (ReaderT CompilerConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig -> IO (Either InterpreterError RunnableConfig)
compileConfig c = flip runReaderT cConfig . runInterpreter . runCompiler $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}
  where cConfig = CompilerConfig {ccSearchPath = cfgPluginDirs c}

compileWatcher :: SerializableWatcher -> Compiler RunnableWatcher
compileWatcher w = do
  mP <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $ compileCode p
  hs <- mapM compileProcessor (wProcessors w)
  return $ w {wPreProcessor=mP, wProcessors=hs}

compileProcessor :: ProcessorCode -> Compiler Processor
compileProcessor (ProcessorCode code)  = compileCode code
compileProcessor (ProcessorShell cmds) = return (executeShellCommands cmds)


executeShellCommands :: [String] -> Processor
executeShellCommands cmds filename content = do
  let env  = envSet "FILENAME" filename mempty
      sh s = executeShellCmd $ (shellCmd s) {shInput=Just content, shEnv=env}
  mapM_ sh cmds

compileCode :: forall a. Typeable a => Code -> Compiler a
compileCode spec = Compiler $ do
  sp <- lift $ asks ccSearchPath
  set [searchPath := sp]
  case spec of
    EvalCode s -> do
      setImportsQ pluginImports
      interpret ('\\':s) as
    ImportCode m s c -> do
      loadModules [m]
      setTopLevelModules [m]
      setImportsQ pluginImports
      let cmd = concat [ "\\c -> case parseEither parseJSON (Object c) of {"
                       , "          Right v -> Right (", s, " v);          "
                       , "          Left e  -> Left e;                     "
                       , "          }                                      "
                       ]
      eO <- interpret cmd as
      case eO c of 
        Right o -> return o
        Left e -> fail $ concat ["Error when compiling ", m, ":", s, ": ", e]
    
pluginImports :: [(ModuleName, Maybe String)]
pluginImports = [
    ("Data.Aeson", Nothing)
  , ("Data.Aeson.Types", Nothing)
  , ("System.DirWatch.PluginAPI", Nothing)
  , ("Data.HashMap.Strict", Nothing)
  , ("Data.Text", Nothing)
  , ("Data.ByteString.Lazy", Nothing)
  , ("Prelude", Nothing)
  ]
