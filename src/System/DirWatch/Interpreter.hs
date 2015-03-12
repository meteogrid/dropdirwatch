{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Interpreter (
  compileConfig
) where

import Control.Monad (forM_)
import Control.Applicative (Applicative)
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (lift)
import Data.Typeable (Typeable)
import Language.Haskell.Interpreter (
    InterpreterT
  , InterpreterError
  , ModuleName
  , runInterpreter
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

data CompilerConfig
  = CompilerConfig {
      ccSearchPath :: [FilePath]
    , ccShellEnv   :: [(String,String)]
  }

newtype Compiler a
  = Compiler {
      runCompiler :: InterpreterT (ReaderT CompilerConfig IO) a
  } deriving (Functor, Applicative, Monad)

compileConfig
  :: SerializableConfig -> IO (Either InterpreterError RunnableConfig)
compileConfig c = flip runReaderT cConfig . runInterpreter . runCompiler $ do
  watchers <- mapM compileWatcher (cfgWatchers c)
  return $ c {cfgWatchers=watchers}
  where cConfig = CompilerConfig {
                    ccSearchPath = cfgPluginDirs c
                  , ccShellEnv   = cfgShellEnv c
                  }

compileWatcher :: SerializableWatcher -> Compiler RunnableWatcher
compileWatcher w = do
  mP <- case wPreProcessor w of
          Nothing -> return Nothing
          Just p  -> fmap (Just . PreProcessor) $ compileCode p
  hs <- mapM compileHandler (wHandlers w)
  return $ w {wPreProcessor=mP, wHandlers=hs}

compileHandler :: HandlerCode -> Compiler Handler
compileHandler (HandlerCode code)  = fmap Handler (compileCode code)
compileHandler (HandlerShell cmds) = executeShellCommands cmds


executeShellCommands :: [String] -> Compiler Handler
executeShellCommands cmds = do
  env <- Compiler . lift . asks $ ccShellEnv
  return $ Handler (handler env)
  where
    handler env filename content
      = forM_ cmds $ \cmd -> do
          let process = (shell cmd)
                { std_in = CreatePipe
                , env    = Just (env ++ [("FILENAME", filename)])
                }
          (Just s_in, _, _, h) <- createProcess process
          LBS.hPut s_in content
          exitCode <- waitForProcess h
          case exitCode of
            ExitSuccess   -> return ()
            ExitFailure c -> error $ concat [ "Shell command", show cmd
                                            , " exited with code ", show c]

compileCode :: forall a. Typeable a => Code -> Compiler a
compileCode spec = Compiler $ do
  sp <- lift $ asks ccSearchPath
  set [searchPath := sp]
  case spec of
    EvalCode s -> do
      setImportsQ pluginImports
      interpret ('\\':s) as
    ImportCode m s c -> do
      loadModules [m]
      setTopLevelModules [m]
      setImportsQ pluginImports
      let cmd = concat [ "\\c -> case parseEither parseJSON (Object c) of {"
                       , "          Right v -> Right (", s, " v);          "
                       , "          Left e  -> Left e;                     "
                       , "          }                                      "
                       ]
      eO <- interpret cmd as
      case eO c of 
        Right o -> return o
        Left e -> fail $ concat ["Error when compiling ", m, ":", s, ": ", e]
    
pluginImports :: [(ModuleName, Maybe String)]
pluginImports = [
    ("Data.Aeson", Nothing)
  , ("Data.Aeson.Types", Nothing)
  , ("System.DirWatch.PluginAPI", Nothing)
  , ("Data.HashMap.Strict", Nothing)
  , ("Data.Text", Nothing)
  , ("Data.ByteString.Lazy", Nothing)
  , ("Prelude", Nothing)
  ]
