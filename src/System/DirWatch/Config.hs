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
  , getCompiled
  , compileWith
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
import qualified Data.Text as T
import qualified Data.List.Split as L
import qualified Data.HashMap.Strict as HM
import System.FilePath.GlobPattern (GlobPattern)
import System.DirWatch.ShellEnv (ShellEnv)
import System.DirWatch.Processor (Processor)
import System.DirWatch.PreProcessor (PreProcessor)

data Config w
  = Config {
      cfgPluginDirs :: [FilePath]
    , cfgArchiveDir :: Maybe FilePath
    , cfgShellEnv   :: ShellEnv
    , cfgWatchers   :: [w]
  } deriving Show

type SerializableConfig = Config SerializableWatcher
type RunnableConfig  = Config RunnableWatcher
type SerializableWatcher = Watcher Code ProcessorCode
type RunnableWatcher
  = Watcher (Compiled PreProcessor Code) (Compiled Processor ProcessorCode)

instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "archiveDir" .= cfgArchiveDir
    , "env"        .= cfgShellEnv
    , "pluginDirs" .= cfgPluginDirs
    , "watchers"   .= cfgWatchers
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .:? "pluginDirs" .!= [] <*>
      v .:? "archiveDir" <*>
      v .:? "env" .!= mempty <*>
      v .:  "watchers"
  parseJSON _ = fail "Expected an object"



data Watcher p h
  = Watcher {
      wName         :: String
    , wPaths        :: [GlobPattern]
    , wPreProcessor :: Maybe p
    , wProcessor    :: Maybe h
  } deriving (Eq, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Watcher a b) where
  toJSON Watcher{..}
    = object [
        "name"         .= wName
      , "paths"        .= wPaths
      , "preprocessor" .= wPreProcessor
      , "processor"    .= wProcessor
    ]

instance FromJSON SerializableWatcher where
  parseJSON (Object v)
    | not (null unexpectedKeys)
    = fail $ "Unexpected keys for watcher: " ++
             T.unpack (T.intercalate ", " unexpectedKeys)
    | otherwise
    = Watcher <$>
      v .:   "name" <*>
      v .:   "paths" <*>
      v .:?  "preprocessor" <*>
      v .:?  "processor"
    where expectedKeys = ["name", "paths", "preprocessor", "processor"]
          unexpectedKeys = filter (`notElem` expectedKeys) $ HM.keys v
  parseJSON _ = fail "Expected an object"

newtype Compiled a code = Compiled (a, code)
getCompiled :: Compiled a code -> a
getCompiled (Compiled a) = fst a

instance Show code => Show (Compiled a code) where
  show (Compiled (_,a)) = show a

instance Eq code => Eq (Compiled a code) where
  Compiled (_,a) == Compiled (_,b) = a==b

compileWith :: Functor m => (code -> m a) -> code -> m (Compiled a code)
compileWith f code = fmap (\a -> Compiled (a,code)) (f code)

instance ToJSON code => ToJSON (Compiled a code) where
  toJSON (Compiled (_,code)) = toJSON code

data Code
  = EvalCode   { codeEval   :: String}
  | ImportCode { codeModule :: String
               , codeSymbol :: String
               , codeParams :: Object}
  deriving (Show, Eq)

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
