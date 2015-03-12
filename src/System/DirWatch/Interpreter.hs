{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch.Interpreter (
  loadConfig
) where

import Control.Monad (forM_)
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
import System.Process (
    CreateProcess(..)
  , StdStream(CreatePipe)
  , shell
  , waitForProcess
  , createProcess
  )
import qualified Data.ByteString.Lazy as LBS
import System.Exit (ExitCode(..))
import System.DirWatch.Config

loadConfig :: SerializableConfig -> IO RunnableConfig
loadConfig c = do
  watchers <- mapM loadWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}

loadWatcher :: SerializableWatcher -> IO RunnableWatcher
loadWatcher w = do
  mP <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap (Just . PreProcessor) $ loadCode p
  hs <- mapM loadHandler (wHandlers w)
  return $ w {wPreProcessor=mP, wHandlers=hs}

loadHandler :: HandlerCode -> IO Handler
loadHandler (HandlerCode code)  = fmap Handler (loadCode code)
loadHandler (HandlerShell cmds) = return $ executeShellCommands cmds


executeShellCommands :: [String] -> Handler
executeShellCommands cmds = Handler handler
  where
    handler filename content
      = forM_ cmds $ \cmd -> do
          let process = (shell cmd)
                { std_in = CreatePipe
                , env    = Just ([("FILENAME", filename)])
                }
          (Just s_in, _, _, h) <- createProcess process
          LBS.hPut s_in content
          exitCode <- waitForProcess h
          case exitCode of
            ExitSuccess   -> return ()
            ExitFailure c -> error $ concat [ "Shell command", show cmd
                                            , " exited with code ", show c]

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
