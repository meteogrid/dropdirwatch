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
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Text (Text, pack)
import System.DirWatch.Logging (fromStrings)
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
      unCompiler :: ExceptT [Text] (ReaderT CompilerConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig -> IO (Either [Text] RunnableConfig)
compileConfig c = runCompiler cConfig $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}
  where cConfig = CompilerConfig {ccSearchPath = cfgPluginDirs c}

runCompiler :: CompilerConfig -> Compiler a -> IO (Either [Text] a)
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
                       , envSearchPath=sp
                       }
  eRet <- liftIO $ case spec of
    EvalCode s ->
      interpret env ('\\':s)
    ImportCode m s c -> do
      let env' = env {envTargets=[m]}
          cmd = concat ["\\c -> case parseEither parseJSON (Object c) of {"
                , "          Right v -> Right (", s, " v);"
                , "          Left e  -> Left e;"
                , "          }"
                ]
      eOut <- interpret env' cmd
      return $ case eOut of
        Right eO ->
          case eO c of 
            Right o -> Right o
            Left e -> Left $
                        ["Error when parsing plugin config: " <> pack e]
        Left es -> Left $
                    (fromStrings ["Error when compiling ", m, ".", s]):es
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
