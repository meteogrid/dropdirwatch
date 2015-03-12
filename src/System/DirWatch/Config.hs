{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module System.DirWatch.Config (
    Config (..)
  , Watcher (..)
  , Code (..)
  , EnvItem (..)
  , ShellEnv (..)
  , HandlerCode (..)
  , Handler (..)
  , PreProcessor (..)
  , SerializableConfig
  , RunnableConfig
  , SerializableWatcher
  , RunnableWatcher
) where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Aeson (
    FromJSON (..)
  , ToJSON (..)
  , Object
  , Value (..)
  , object
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Text (Text, pack, unpack, uncons)
import Data.Typeable (Typeable)
import qualified Data.List.Split as L
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import System.FilePath.GlobPattern (GlobPattern)

data Config p h
  = Config {
      cfgPluginDirs :: [FilePath]
    , cfgShellEnv   :: ShellEnv
    , cfgWatchers   :: [Watcher p h]
  }

type SerializableConfig = Config Code HandlerCode
type RunnableConfig      = Config PreProcessor Handler

type SerializableWatcher = Watcher Code HandlerCode
type RunnableWatcher     = Watcher PreProcessor Handler

newtype ShellEnv = ShellEnv [EnvItem] deriving (Show, Eq)

instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "watchers"   .= cfgWatchers
    , "pluginDirs" .= cfgPluginDirs
    , "env"        .= cfgShellEnv
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .:? "pluginDirs" .!= [] <*>
      v .:? "env" .!= ShellEnv [] <*>
      v .:  "watchers"
  parseJSON _ = fail "Expected an object"

data EnvItem
  = EnvSet    {envKey :: String, envVal :: String}
  | EnvAppend {envKey :: String, envVal :: String}
  deriving (Show, Eq)

instance ToJSON ShellEnv where
  toJSON (ShellEnv items) = object $ map toPair items
    where toPair (EnvSet k v)    = (pack k, toJSON v)
          toPair (EnvAppend k v) = (pack ('+':k), toJSON v)

instance FromJSON ShellEnv where
  parseJSON (Object o)
    = ShellEnv <$> mapM fromPair (HM.toList o)
    where
      fromPair (k, v) = case uncons k of
        Just ('+',k') -> EnvAppend <$> pure (unpack k') <*> parseJSON v
        Just _        -> EnvSet    <$> pure (unpack k)  <*> parseJSON v
        Nothing       -> fail "Unexpected empty jey for ShellEnv"
  parseJSON _ = fail "Expected for env"

data Watcher p h
  = Watcher {
      wName         :: Text
    , wPaths        :: [GlobPattern]
    , wPreProcessor :: Maybe p
    , wHandlers     :: [h]
  }

instance ToJSON SerializableWatcher where
  toJSON Watcher{..}
    = object [
        "name"         .= wName
      , "paths"        .= wPaths
      , "preprocessor" .= wPreProcessor
      , "handlers"     .= wHandlers
    ]

instance FromJSON SerializableWatcher where
  parseJSON (Object v)
      = Watcher <$>
        v .:   "name" <*>
        v .:   "paths" <*>
        v .:?  "preprocessor" <*>
        v .:?  "handlers" .!= []
  parseJSON _ = fail "Expected an object"

data Code
  = EvalCode   String
  | ImportCode String String Object
  deriving (Show)

instance ToJSON Code where
  toJSON (EvalCode s)
    = object ["eval" .= s]
  toJSON (ImportCode m s c)
    = Object (HM.union os c)
    where
      os = case object ["import" .= concat [m, ":", s]] of
            Object os' -> os'
            _          -> error "should never happen"

instance FromJSON Code where
  parseJSON (Object v)
      = (EvalCode <$> v .: "eval")
    <|> (do i <- v .: "import"
            case L.splitOn ":" i of
             [m,s] -> return (ImportCode m s (HM.delete "import" v))
             _     -> fail "\"import\" should be <module>:<symbol>")
  parseJSON _ = fail "Expected an object"

data HandlerCode
  = HandlerCode  Code
  | HandlerShell [String]
  deriving Show

instance ToJSON HandlerCode where
  toJSON (HandlerCode  c) = toJSON c
  toJSON (HandlerShell c) = object ["shell" .= toJSON c]

instance FromJSON HandlerCode where
  parseJSON o@(Object v)
    = (HandlerShell <$> v .: "shell") <|> (HandlerCode <$> parseJSON o)
  parseJSON _ = fail "Expected an object"


newtype Handler
  = Handler {
      handle :: FilePath -> LBS.ByteString -> IO ()
  } deriving Typeable

newtype PreProcessor
  = PreProcessor {
      preProcess :: FilePath -> LBS.ByteString -> [(FilePath,LBS.ByteString)]
  } deriving Typeable
