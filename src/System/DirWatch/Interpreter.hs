{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.IO.Class(liftIO)
import Data.Typeable (Typeable)
import Data.Aeson (Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import System.DirWatch.Config (
    Config(..)
  , Watcher(..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableConfig
  , RunnableWatcher
  , Code (..)
  , ProcessorCode (..)
  , compileWith
  )
import System.DirWatch.Processor (Processor, shellProcessor)
import System.DirWatch.Compiler

data CompilerConfig
  = CompilerConfig {
      ccSearchPath :: [FilePath]
  }

newtype Compiler a
  = Compiler {
      unCompiler :: ExceptT ByteString (ReaderT CompilerConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig -> IO (Either ByteString RunnableConfig)
compileConfig c = runCompiler cConfig $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}
  where cConfig = CompilerConfig {ccSearchPath = cfgPluginDirs c}

runCompiler :: CompilerConfig -> Compiler a -> IO (Either ByteString a)
runCompiler c = flip runReaderT c . runExceptT . unCompiler


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
  let env = defaultEnv { envImports=map fst pluginImports
                       , importPaths=sp
                       }
  eRet <- liftIO $ case spec of
    EvalCode s ->
      interpret env ("let sym = \\"<>s<>" in sym")
    ImportCode m s c -> do
      let env' = env {envTargets=[m]}
          cmd = concat [
                  "let sym = \\c -> case parseEither parseJSON (Object c) of {"
                , "          Right v -> Right (", s, " v);"
                , "          Left e  -> Left e;"
                , "          }"
                , "in sym"
                ]
      eOut <- interpret env' cmd
      return $ case eOut of
        Right eO ->
          case eO c of 
            Right o -> Right o
            Left e -> Left $ "Error when parsing plugin config: " <> BS.pack e
        Left e -> Left e
  either throwE return eRet
    
pluginImports :: [(String, Maybe String)]
pluginImports = [
    ("Data.Aeson", Nothing)
  , ("Data.Aeson.Types", Nothing)
  , ("System.DirWatch.PluginAPI", Nothing)
  , ("Data.HashMap.Strict", Nothing)
  , ("Data.Text", Nothing)
  , ("Data.ByteString", Nothing)
  , ("Control.Monad.Trans.Resource", Nothing)
  , ("Data.Conduit", Nothing)
  , ("Prelude", Nothing)
  ]
