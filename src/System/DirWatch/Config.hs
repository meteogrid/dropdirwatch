{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module System.DirWatch.Config (
    Config (..)
  , Watcher (..)
  , Code (..)
  , ProcessorCode (..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableWatcher
  , RunnableConfig
) where

import Control.Applicative ((<$>), (<*>), (<|>))
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
import Data.Monoid (Monoid(..))
import qualified Data.List.Split as L
import qualified Data.HashMap.Strict as HM
import System.FilePath.GlobPattern (GlobPattern)
import System.DirWatch.ShellEnv (ShellEnv)
import System.DirWatch.Processor (PreProcessor, Processor)

data Config p h
  = Config {
      cfgPluginDirs :: [FilePath]
    , cfgShellEnv   :: ShellEnv
    , cfgWatchers   :: [Watcher p h]
  }

type SerializableConfig = Config Code ProcessorCode
type SerializableWatcher = Watcher Code ProcessorCode
type RunnableConfig  = Config PreProcessor Processor
type RunnableWatcher = Watcher PreProcessor Processor


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
      v .:? "env" .!= mempty <*>
      v .:  "watchers"
  parseJSON _ = fail "Expected an object"



data Watcher p h
  = Watcher {
      wName         :: String
    , wPaths        :: [GlobPattern]
    , wPreProcessor :: Maybe p
    , wProcessors   :: [h]
  }

instance ToJSON SerializableWatcher where
  toJSON Watcher{..}
    = object [
        "name"         .= wName
      , "paths"        .= wPaths
      , "preprocessor" .= wPreProcessor
      , "processors"   .= wProcessors
    ]

instance FromJSON SerializableWatcher where
  parseJSON (Object v)
      = Watcher <$>
        v .:   "name" <*>
        v .:   "paths" <*>
        v .:?  "preprocessor" <*>
        v .:?  "processors" .!= []
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
      = (do c <- v .: "eval"
            if HM.size v == 1
              then return $ EvalCode c
              else fail "\"eval\" expects no arguments")
    <|> (do i <- v .: "import"
            case L.splitOn ":" i of
             [m,s] -> return (ImportCode m s (HM.delete "import" v))
             _     -> fail "\"import\" should be <module>:<symbol>")
  parseJSON _ = fail "Expected an object"

data ProcessorCode
  = ProcessorCode  Code
  | ProcessorShell [String]
  deriving Show

instance ToJSON ProcessorCode where
  toJSON (ProcessorCode  c) = toJSON c
  toJSON (ProcessorShell c) = object ["shell" .= toJSON c]

instance FromJSON ProcessorCode where
  parseJSON o@(Object v)
    = (ProcessorShell <$> v .: "shell") <|> (ProcessorCode <$> parseJSON o)
  parseJSON _ = fail "Expected an object"
