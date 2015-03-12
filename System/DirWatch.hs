{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module System.DirWatch where

import Control.Applicative (pure, (<$>), (<*>))
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
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import System.FilePath.GlobPattern (GlobPattern)
import Language.Haskell.Interpreter
import Prelude

data Config p h
  = Config {
      cfgWatchers :: [Watcher p h]
  }

type SerializableConfig = Config Spec Spec
type RunableConfig      = Config PreProcessor Handler

type SerializableWatcher = Watcher Spec Spec
type RunableWatcher      = Watcher PreProcessor Handler

instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "watchers" .= cfgWatchers
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .: "watchers"
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

data Spec
  = EvalSpec   String
  | ImportSpec String c
  deriving (Show)

instance ToJSON Spec where
  toJSON (EvalSpec s)
    = object ["eval" .= s]
  toJSON (ImportSpec s c)
    = Object (HM.union os c)
    where
      os = case object ["import" .= s] of
            Object os' -> os'
            _          -> error "should never happen"

instance FromJSON Spec where
  parseJSON (Object v) = do
    mEval <- v .:? "eval"
    case mEval of
      Just s  -> return $ EvalSpec s
      Nothing -> ImportSpec <$>
                 v .: "import"
                 <*> (pure (HM.delete "import" v))
  parseJSON _ = fail "Expected an object"

loadConfig :: SerializableConfig -> IO RunableConfig
loadConfig c = do
  watchers <- mapM loadWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}

loadWatcher :: SerializableWatcher -> IO RunableWatcher
loadWatcher w = do
  mP <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap Just $ loadSpec p
  hs <- mapM loadSpec (wHandlers w)
  return $ w {wPreProcessor=mP, wHandlers=hs}

loadSpec :: Typeable a => Spec -> IO a
loadSpec spec = do
  result <- runInterpreter $ do
    setImportsQ [("Prelude", Nothing)]
    case spec of
      EvalSpec s -> interpret s as
  case result of
    Left  e -> error $ show e
    Right v -> return v
    

newtype Handler
  = Handler {
      handle :: FilePath -> LBS.ByteString -> IO ()
  } deriving Typeable

newtype PreProcessor
  = PreProcessor {
      preProcess :: FilePath -> LBS.ByteString -> [(FilePath,LBS.ByteString)]
  } deriving Typeable
