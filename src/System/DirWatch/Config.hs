{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Config (
    Config (..)
  , Watcher (..)
  , Code (..)
  , Compiled
  , SymOrCode (..)
  , SymbolTable (..)
  , ProcessorCode (..)
  , ModuleImport (..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableWatcher
  , RunnableConfig
  , getCompiled
  , compileWith
) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (foldM_)
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
import System.DirWatch.ShellEnv (ShellEnv)
import System.DirWatch.Processor (Processor, ProcessorM)
import System.DirWatch.PreProcessor (PreProcessor)
import System.DirWatch.Util (AbsPath)
import System.FilePath.GlobPattern (GlobPattern)

data Config pp p ppc pc
  = Config {
      cfgPluginDirs    :: [FilePath]
    , cfgArchiveDir    :: Maybe AbsPath
    , cfgShellEnv      :: ShellEnv
    , cfgWatchers      :: [Watcher pp p]
    , cfgWaitSeconds   :: Int
    , cfgPackageDbs    :: [GlobPattern]
    , cfgImports       :: [ModuleImport]
    , cfgProcessors    :: SymbolTable pc
    , cfgPreProcessors :: SymbolTable ppc
  } deriving Show

type SerializableConfig
  = Config (SymOrCode Code) (SymOrCode ProcessorCode)
           Code             ProcessorCode
type SerializableWatcher = Watcher (SymOrCode Code) (SymOrCode ProcessorCode)
type RunnableConfig
  = Config (Compiled (PreProcessor ProcessorM) Code)
           (Compiled Processor ProcessorCode)
           Code
           ProcessorCode
type RunnableWatcher
  = Watcher (Compiled (PreProcessor ProcessorM) Code)
            (Compiled Processor ProcessorCode)

newtype SymbolTable p
  = SymbolTable (HM.HashMap String p) deriving (Show, Eq, ToJSON, FromJSON)


instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "archiveDir"    .= cfgArchiveDir
    , "env"           .= cfgShellEnv
    , "pluginDirs"    .= cfgPluginDirs
    , "watchers"      .= cfgWatchers
    , "waitSeconds"   .= cfgWaitSeconds
    , "imports"       .= cfgImports
    , "processors"    .= cfgProcessors
    , "preprocessors" .= cfgPreProcessors
    , "packageDbs"    .= cfgPackageDbs
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .:? "pluginDirs" .!= [] <*>
      v .:? "archiveDir" <*>
      v .:? "env" .!= mempty <*>
      (v .: "watchers" >>= failIfDuplicate "Duplicate watcher" wName) <*>
      v .:? "waitSeconds" .!= 60 <*>
      v .:? "packageDbs" .!= [] <*>
      v .:? "imports" .!= [] <*>
      v .:? "processors"    .!= SymbolTable HM.empty <*>
      v .:? "preprocessors" .!= SymbolTable HM.empty
  parseJSON _ = fail "Expected an object for \"config\""

failIfDuplicate
  :: (Monad m, Eq b, Show b) => String -> (a -> b) -> [a] -> m [a]
failIfDuplicate msg func objs =  foldM_ check [] (map func objs) >> return objs
  where
    check acc o
      | o `elem` acc = fail $ concat [msg, ": ", show o]
      | otherwise    = return (o:acc)

data SymOrCode code
  = SymName  String
  | SymCode  code
  deriving (Eq, Show)

data Watcher pp p
  = Watcher {
      wName         :: String
    , wPaths        :: [AbsPath]
    , wPreProcessor :: Maybe pp
    , wProcessor    :: Maybe p
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
  parseJSON _ = fail "Expected an object for \"watcher\""

instance ToJSON code => ToJSON (SymOrCode code) where
  toJSON (SymName name) = toJSON name
  toJSON (SymCode code) = toJSON code

instance FromJSON code => FromJSON (SymOrCode code) where
  parseJSON o@Object{} = SymCode <$> parseJSON o
  parseJSON s@String{} = SymName <$> parseJSON s
  parseJSON _          = fail "Must be either string or object"


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

newtype ModuleImport
  = ModuleImport {unModuleImport :: (String, Maybe String)}
  deriving (Show, Eq)

instance ToJSON ModuleImport where
  toJSON (ModuleImport (m,Nothing)) = toJSON m
  toJSON (ModuleImport (m,Just qual)) = toJSON $ concat [m, " as ", qual]

instance FromJSON ModuleImport where
  parseJSON (String s)
    = case L.splitOn " as " (T.unpack s) of
        [m,qual] -> return (ModuleImport (m,Just qual))
        [m]      -> return (ModuleImport (m,Nothing))
        _ -> fail "\"imports\" should be: module-name [as qualified-name]"
  parseJSON _ = fail "Expected a string for \"imports\""

data Code
  = EvalCode          { codeEval    :: String
                      , codeImports :: [ModuleImport]}
  | InterpretedPlugin { codeModule :: String
                      , codeSymbol :: String
                      , codeParams :: Object}
  | LoadedPlugin      { codeSymbol :: String
                      , codeParams :: Object}
  deriving (Show, Eq)

instance ToJSON Code where
  toJSON EvalCode{..}
    = object ["eval" .= codeEval, "imports" .= codeImports]
  toJSON LoadedPlugin{..}
    = Object (HM.union os codeParams)
    where
      os = case object ["plugin" .= codeSymbol] of
            Object os' -> os'
            _          -> error "should never happen"
  toJSON InterpretedPlugin{..}
    = Object (HM.union os codeParams)
    where
      os = case object ["plugin" .= concat [codeModule, ":", codeSymbol]] of
            Object os' -> os'
            _          -> error "should never happen"

instance FromJSON Code where
  parseJSON (Object v) = do
    mCode <- v .:? "eval"
    case mCode of
      Just code -> do
        imports <- v .:? "imports" .!= []
        if HM.size v <=2
          then return $ EvalCode code imports
          else fail "\"eval\" expects only \"imports\" as an optional arg"
      Nothing -> do
        plugin <- v .: "plugin"
        case L.splitOn ":" plugin of
          [modname,symbol] ->
            return (InterpretedPlugin modname symbol (HM.delete "plugin" v))
          [symbol] ->
            return (LoadedPlugin symbol (HM.delete "plugin" v))
          _     -> fail "\"plugin\" should be <module>:<symbol>"
  parseJSON _ = fail "Expected an object for \"eval\" or \"plugin\""

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
  parseJSON _ = fail "Expected an object for \"eval\", \"shell\" or \"plugin\""
