{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch.Interpreter (
  loadConfig
) where

import Data.Typeable (Typeable)
import Language.Haskell.Interpreter (
    runInterpreter
  , interpret
  , as
  , setImportsQ
  , loadModules
  , setTopLevelModules
  , set
  , searchPath
  , OptionVal ((:=))
  )
import System.DirWatch.Config

loadConfig :: SerializableConfig -> IO RunableConfig
loadConfig c = do
  watchers <- mapM loadWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}

loadWatcher :: SerializableWatcher -> IO RunableWatcher
loadWatcher w = do
  mP <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap (Just . PreProcessor) $ loadCode p
  hs <- mapM (fmap Handler . loadCode) (wHandlers w)
  return $ w {wPreProcessor=mP, wHandlers=hs}

loadCode :: forall a. Typeable a => Code -> IO a
loadCode spec = do
  result <- runInterpreter $ do
    set [searchPath := ["test/plugins"]]
    case spec of
      EvalCode s -> do
        pluginImports
        interpret s as
      ImportCode m s c -> do
        loadModules [m]
        setTopLevelModules [m]
        pluginImports
        let cmd = concat [ "\\c -> case parseEither parseJSON (Object c) of {"
                         , "          Right v -> Right (", s, " v);          "
                         , "          Left e  -> Left e;                     "
                         , "          }                                      "
                         ]
        eO <- interpret cmd as
        case eO c of 
          Right o -> return o
          Left e -> fail $ concat ["Error when loading ", m, ":", s, ": ", e]
  case result of
    Left  e -> error $ show e
    Right v -> return v
    
pluginImports = setImportsQ [
    ("Data.Aeson", Nothing)
  , ("Data.Aeson.Types", Nothing)
  , ("System.DirWatch.PluginAPI", Nothing)
  , ("Data.HashMap.Strict", Nothing)
  , ("Data.Text", Nothing)
  , ("Data.ByteString.Lazy", Nothing)
  , ("Prelude", Nothing)
  ]
