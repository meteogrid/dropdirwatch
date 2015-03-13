{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module System.DirWatch.Handler (
    Handler (..)
  , HandlerM
  , HandlerEnv (..)
  , HandlerError (..)
  , ShellCmd (..)
  , shellCmd
  , executeShellCmd
  , liftIO
  , runHandlerM
  , throwE
  , catchE
) where

import Control.Applicative (Applicative)
import Control.Exception.Lifted as E (
    Exception
  , SomeException
  , handle
  )
import Control.Monad.Reader (MonadReader(ask), asks, ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Control.Monad.Trans.Except as E
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Default (Default(def))
import Data.Monoid (mempty, mappend)
import Data.Typeable (Typeable)
import System.DirWatch.Logging (MonadLogger, LoggingT, runStderrLoggingT)
import System.DirWatch.ShellEnv
import System.Exit (ExitCode(..))
import System.Process (
    CreateProcess(..)
  , StdStream(..)
  , shell
  , waitForProcess
  , createProcess
  )

newtype Handler
  = Handler {
      handle :: FilePath -> LBS.ByteString -> HandlerM ()
  } deriving Typeable

data HandlerEnv
  = HandlerEnv {
      hShellEnv :: ShellEnv
  }

instance Default HandlerEnv where
  def = HandlerEnv mempty

  
newtype HandlerM a
  = HandlerM {
      unHandlerM :: ExceptT HandlerError (LoggingT (ReaderT HandlerEnv IO )) a
      }
  deriving ( Functor, Applicative, Monad, MonadLogger, MonadReader HandlerEnv
           , MonadIO, Typeable)

runHandlerM :: HandlerEnv -> HandlerM a -> IO (Either HandlerError a)
runHandlerM env
  = flip runReaderT env
  . runStderrLoggingT
  . runExceptT
  . intercept
  . unHandlerM

data ShellCmd
  = ShellCmd {
      shCmd   :: String
    , shInput :: Maybe (LBS.ByteString)
    , shEnv   :: ShellEnv
  }

shellCmd :: String -> ShellCmd
shellCmd cmd = ShellCmd cmd Nothing mempty


executeShellCmd :: ShellCmd -> HandlerM (BS.ByteString, BS.ByteString)
executeShellCmd ShellCmd{..} = do
  env <- fmap (shellEnvToEnv . (`mappend` shEnv)) (asks hShellEnv)
  let process = (shell shCmd)
        { std_in  = maybe Inherit (const CreatePipe) shInput
        , std_out = CreatePipe
        , std_err = CreatePipe
        , env     = Just env
        }
  (exitCode, out, err) <- do
    p <- liftIO $ createProcess process
    case (p, shInput) of
      ((Just s_in, Just s_out, Just s_err, ph), Just input) -> liftIO $ do
        LBS.hPut s_in input
        exitCode <- waitForProcess ph
        out <- BS.hGetContents s_out
        err <- BS.hGetContents s_err
        return (exitCode, out, err)
      ((_, Just s_out, Just s_err, ph), Nothing) -> liftIO $ do
        exitCode <- waitForProcess ph
        out <- BS.hGetContents s_out
        err <- BS.hGetContents s_err
        return (exitCode, out, err)
      _ -> throwE $ InternalError "Unexpected state when creating process"
  case exitCode of
    ExitSuccess   -> return (out, err)
    ExitFailure c -> throwE (ShellError c out err)

data HandlerError
  = HandlerException SomeException
  | ShellError {
      seCode   :: Int
    , seStdout :: BS.ByteString
    , seStdErr :: BS.ByteString
    }
  | InternalError String
  deriving (Show, Typeable)

instance Exception HandlerError

throwE :: HandlerError -> HandlerM a
throwE = HandlerM . E.throwE

catchE :: HandlerM a -> (HandlerError -> HandlerM a) -> HandlerM a
catchE act handler = do
  env <- ask
  either handler return =<< liftIO (runHandlerM env act)

intercept
  :: ExceptT HandlerError (LoggingT (ReaderT HandlerEnv IO)) a
  -> ExceptT HandlerError (LoggingT (ReaderT HandlerEnv IO)) a
intercept = E.handle (E.throwE . HandlerException)
