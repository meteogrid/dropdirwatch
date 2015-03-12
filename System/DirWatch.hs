{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
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
import qualified Data.HashMap.Strict as HM
import System.FilePath.GlobPattern (GlobPattern)
import Prelude

data Config
  = Config {
      cfgWatchers :: [Watcher]
  }

instance ToJSON Config where
  toJSON Config{..}
    = object [
      "watchers" .= cfgWatchers
    ]

instance FromJSON Config where
  parseJSON (Object v)
    = Config <$>
      v .: "watchers"
  parseJSON _ = fail "Expected an object"

data Watcher
  = Watcher {
      wName         :: Text
    , wPaths        :: [GlobPattern]
    , wPreProcessor :: Maybe PreProcessor
    , wHandlers     :: [Handler]
  }

instance ToJSON Watcher where
  toJSON Watcher{..}
    = object [
        "name"         .= wName
      , "paths"        .= wPaths
      , "preprocessor" .= wPreProcessor
      , "handlers"     .= wHandlers
    ]

instance FromJSON Watcher where
  parseJSON (Object v)
      = Watcher <$>
        v .:  "name" <*>
        v .:  "paths" <*>
        v .:  "preprocessor" <*>
        v .:? "handlers" .!= []
  parseJSON _ = fail "Expected an object"

data PreProcessor
  = EvalPreProcessor   String
  | ImportPreProcessor String Object
  deriving (Show)

instance ToJSON PreProcessor where
  toJSON (EvalPreProcessor s)
    = object ["eval" .= s]
  toJSON (ImportPreProcessor s c)
    = Object (HM.union os c)
    where
      os = case object ["import" .= s] of
            Object os' -> os'
            _          -> error "should never happen"

instance FromJSON PreProcessor where
  parseJSON (Object v) = do
    mEval <- v .:? "eval"
    case mEval of
      Just s  -> return $ EvalPreProcessor s
      Nothing -> ImportPreProcessor <$>
                 v .: "import"
                 <*> (pure (HM.delete "import" v))
  parseJSON _ = fail "Expected an object"

data Handler = Handler
instance ToJSON Handler where
  toJSON = undefined

instance FromJSON Handler where
  parseJSON = undefined
