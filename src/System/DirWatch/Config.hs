{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module System.DirWatch.Config (
    Config (..)
  , Watcher (..)
  , Code (..)
  , Handler (..)
  , PreProcessor (..)
  , SerializableConfig
  , RunableConfig
  , SerializableWatcher
  , RunableWatcher
) where

import Control.Applicative ((<$>), (<*>))
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
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.List.Split as L
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import System.FilePath.GlobPattern (GlobPattern)

data Config p h
  = Config {
      cfgPluginDirs :: [FilePath]
    , cfgWatchers   :: [Watcher p h]
  }

type SerializableConfig = Config Code Code
type RunableConfig      = Config PreProcessor Handler

type SerializableWatcher = Watcher Code Code
type RunableWatcher      = Watcher PreProcessor Handler

instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "watchers"   .= cfgWatchers
    , "pluginDirs" .= cfgPluginDirs
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .:? "pluginDirs" .!= [] <*>
      v .:  "watchers"
  parseJSON _ = fail "Expected an object"

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
        v .:  "name" <*>
        v .:  "paths" <*>
        v .:  "preprocessor" <*>
        v .:? "handlers" .!= []
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
  parseJSON (Object v) = do
    mEval <- v .:? "eval"
    case mEval of
      Just s  -> return $ EvalCode s
      Nothing -> do
        i <- v .: "import"
        case L.splitOn ":" i of
          [m,s] -> return (ImportCode m s (HM.delete "import" v))
          _     -> fail "\"import\" should be <module>:<symbol>"
  parseJSON _ = fail "Expected an object"

newtype Handler
  = Handler {
      handle :: FilePath -> LBS.ByteString -> IO ()
  } deriving Typeable

newtype PreProcessor
  = PreProcessor {
      preProcess :: FilePath -> LBS.ByteString -> [(FilePath,LBS.ByteString)]
  } deriving Typeable
