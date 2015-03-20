{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Config (
    Config (..)
  , Watcher (..)
  , WatchedPath (..)
  , Code (..)
  , SymOrCode (..)
  , SymbolTable (..)
  , ProcessorCode (..)
  , ModuleImport (..)
  , SerializableConfig
  , SerializableWatcher
  , RunnableWatcher
  , RunnableConfig
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
import Data.Function (on)
import Data.Default (Default(def))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.List.Split as L
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(hashWithSalt))
import Data.Time (NominalDiffTime)
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
    , cfgStableTime    :: NominalDiffTime
    , cfgPackageDbs    :: [GlobPattern]
    , cfgImports       :: [ModuleImport]
    , cfgProcessors    :: SymbolTable pc
    , cfgPreProcessors :: SymbolTable ppc
    , cfgNumRetries    :: Int
    , cfgRetryInterval :: NominalDiffTime
  } deriving Show

instance Default (Config a b c d) where
  def = Config {
        cfgPluginDirs    = mempty
      , cfgArchiveDir    = Nothing
      , cfgShellEnv      = mempty
      , cfgWatchers      = mempty
      , cfgStableTime    = 60
      , cfgPackageDbs    = mempty
      , cfgImports       = mempty
      , cfgProcessors    = SymbolTable HM.empty 
      , cfgPreProcessors = SymbolTable HM.empty
      , cfgNumRetries    = 12
      , cfgRetryInterval = 300
      }

type SerializableConfig
  = Config (SymOrCode Code) (SymOrCode ProcessorCode)
           Code             ProcessorCode
type SerializableWatcher = Watcher (SymOrCode Code) (SymOrCode ProcessorCode)
type RunnableConfig
  = Config (PreProcessor ProcessorM) Processor Code ProcessorCode
type RunnableWatcher = Watcher (PreProcessor ProcessorM) Processor

newtype SymbolTable p
  = SymbolTable (HM.HashMap String p) deriving (Show, Eq, ToJSON, FromJSON)


instance ToJSON SerializableConfig where
  toJSON Config{..}
    = object [
      "archiveDir"    .= cfgArchiveDir
    , "env"           .= cfgShellEnv
    , "pluginDirs"    .= cfgPluginDirs
    , "watchers"      .= cfgWatchers
    , "stableTime"    .= (realToFrac cfgStableTime :: Double)
    , "packageDbs"    .= cfgPackageDbs
    , "imports"       .= cfgImports
    , "processors"    .= cfgProcessors
    , "preprocessors" .= cfgPreProcessors
    , "numRetries"    .= cfgNumRetries
    , "retryInterval" .= (realToFrac cfgRetryInterval :: Double)
    ]

instance FromJSON SerializableConfig where
  parseJSON (Object v)
    = Config <$>
      v .:? "pluginDirs"    .!= cfgPluginDirs def <*>
      v .:? "archiveDir" <*>
      v .:? "env"           .!= cfgShellEnv def <*>
      (v .: "watchers" >>= failIfDuplicate "Duplicate watcher" wName) <*>
      maybeDiffTime (v .:? "stableTime") (cfgStableTime def) <*>
      v .:? "packageDbs"    .!= cfgPackageDbs def <*>
      v .:? "imports"       .!= cfgImports def <*>
      v .:? "processors"    .!= cfgProcessors def <*>
      v .:? "preprocessors" .!= cfgPreProcessors def <*>
      v .:? "numRetries"    .!= cfgNumRetries def <*>
      maybeDiffTime (v .:? "retryInterval") (cfgRetryInterval def)
  parseJSON _ = fail "Expected an object for \"config\""

maybeDiffTime
  :: Functor m
  => m (Maybe Double) -> NominalDiffTime -> m NominalDiffTime
maybeDiffTime p dflt = fmap (fromMaybe dflt . fmap realToFrac) p

failIfDuplicate
  :: (Monad m, Eq b, Show b) => String -> (a -> b) -> [a] -> m [a]
failIfDuplicate msg func objs =  foldM_ check [] (map func objs) >> return objs
  where
    check acc o
      | o `elem` acc = fail $ concat [msg, ": ", show o]
      | otherwise    = return (o:acc)

data Watcher pp p
  = Watcher {
      wName         :: String
    , wPaths        :: [WatchedPath pp]
    , wProcessor    :: Maybe p
  } deriving Show

instance Eq (Watcher a b) where
  (==) = (==) `on` wName
instance Hashable (Watcher a b) where
  hashWithSalt n = hashWithSalt n . wName

instance (ToJSON a, ToJSON b) => ToJSON (Watcher a b) where
  toJSON Watcher{..}
    = object [
        "name"         .= wName
      , "paths"        .= wPaths
      , "processor"    .= wProcessor
    ]

instance FromJSON SerializableWatcher where
  parseJSON (Object v)
    | not (null unexpectedKeys)
    = fail $ "Unexpected keys for watcher: " ++
             T.unpack (T.intercalate ", " unexpectedKeys)
    | otherwise = do
        name   <- v .: "name"
        paths  <- v .: "paths"
        pp     <- v .:?  "preprocessor"
        proc   <- v .:?  "processor"
        return $ Watcher {
            wName      = name
          , wPaths     = case pp of
                           Just pp' -> map (assignPp pp') paths
                           Nothing  -> paths
          , wProcessor = proc
          }
    where expectedKeys = ["name", "paths", "preprocessor", "processor"]
          unexpectedKeys = filter (`notElem` expectedKeys) $ HM.keys v
          assignPp pp wp@WatchedPath{wpPreprocessor=mPp}
            = wp {wpPreprocessor=mPp <|> Just pp}
  parseJSON _ = fail "Expected an object for \"watcher\""

data WatchedPath pp
  = WatchedPath {
      wpGlob         :: AbsPath
    , wpPreprocessor :: Maybe pp
    }
  deriving (Show, Eq)

instance ToJSON pp => ToJSON (WatchedPath pp) where
  toJSON WatchedPath{wpGlob=p, wpPreprocessor=Nothing} = toJSON p
  toJSON WatchedPath{..} = toJSON $ object [ "pattern"         .= wpGlob
                                           , "preprocessor" .= wpPreprocessor]

instance FromJSON pp => FromJSON (WatchedPath pp) where
  parseJSON (Object v) = do
    path <- v .: "pattern"
    pp <- v .:? "preprocessor"
    return $ WatchedPath path pp
  parseJSON s@String{} = do
    path <- parseJSON s
    return $ WatchedPath path Nothing
  parseJSON _ = fail "\"paths\" item must be either an object or string"

data SymOrCode code
  = SymName  String
  | SymCode  code
  deriving (Eq, Show)

instance ToJSON code => ToJSON (SymOrCode code) where
  toJSON (SymName name) = toJSON name
  toJSON (SymCode code) = toJSON code

instance FromJSON code => FromJSON (SymOrCode code) where
  parseJSON o@Object{} = SymCode <$> parseJSON o
  parseJSON s@String{} = SymName <$> parseJSON s
  parseJSON _          = fail "Must be either string or object"

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
