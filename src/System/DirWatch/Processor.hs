{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.DirWatch.Processor (
    Processor
  , ProcessorM
  , ProcessorConduit
  , ProcessorSource
  , ProcessorConfig (..)
  , ProcessorError (..)
  , ShellCmd (..)
  , shellCmd
  , executeShellCmd
  , shellProcessor
  , createProcess
  , waitForProcess
  , forkChild
  , killChild
  , waitChild
  , tryWaitChild
  , liftIO
  , runProcessorM
  , throwE
  , catchE
) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception.Lifted as E (
    Exception
  , SomeException
  , IOException
  , handle
  , catch
  )
import Control.Monad (void)
import Control.Monad.Reader (MonadReader(..), asks, ReaderT, runReaderT)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Resource (
    MonadResource
  , ResourceT
  , runResourceT
  )
import qualified Control.Monad.Trans.Except as E
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Conduit (Conduit, ConduitM, Source, ($$))
import Data.Conduit.Binary (sinkHandle)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default(def))
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Monoid (mempty, mappend)
import Data.Typeable (Typeable)
import System.DirWatch.Logging (
    MonadLogger
  , LoggingT
  , runStderrLoggingT
  )
import System.DirWatch.ShellEnv
import System.DirWatch.Threading (ThreadHandle, SomeThreadHandle)
import qualified System.DirWatch.Threading as Th
import System.Exit (ExitCode(..))
import System.Process (
    CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , shell
  , interruptProcessGroupOf
  , terminateProcess
  , getProcessExitCode
  )
import qualified System.Process as P (createProcess, waitForProcess)
import System.IO (Handle)

type ProcessorConduit i o = Conduit i ProcessorM o
type ProcessorSource o = Source ProcessorM o

type Processor = FilePath -> ProcessorSource ByteString -> ProcessorM ()

data ProcessorConfig
  = ProcessorConfig {
      pShellEnv :: ShellEnv
  }

data ProcessorEnv
  = ProcessorEnv {
      pConfig   :: ProcessorConfig
    , pProcs    :: IORef [ProcessHandle]
    , pThreads  :: IORef [SomeThreadHandle ProcessorError]
    }


instance Default ProcessorConfig where
  def = ProcessorConfig mempty

  
newtype ProcessorM a
  = ProcessorM {
      unProcessorM :: ExceptT ProcessorError (
        LoggingT (ResourceT (ReaderT ProcessorEnv IO))
        ) a
      }
  deriving ( Functor, Applicative, Monad, MonadLogger
           , MonadIO, MonadBase IO, Typeable, MonadThrow, MonadResource)

runProcessorEnv :: ProcessorEnv -> ProcessorM a -> IO (Either ProcessorError a)
runProcessorEnv env
  = flip runReaderT env
  . runResourceT
  . runStderrLoggingT
  . runExceptT
  . E.handle (E.throwE . ProcessorException)
  . unProcessorM

deriving instance Typeable ConduitM

instance (MonadReader ProcessorConfig) ProcessorM where
  ask = ProcessorM (asks pConfig)
  local f act = do
    env <- ProcessorM ask
    let env' = env {pConfig = f (pConfig env)}
    liftIO (runProcessorEnv env' act) >>= restoreM
    

instance MonadBaseControl IO ProcessorM where
   type StM ProcessorM a = Either ProcessorError a
   liftBaseWith f = do
     env <- ProcessorM ask
     liftIO $ f (runProcessorEnv env)
   restoreM = either throwE return

runProcessorM :: ProcessorConfig -> ProcessorM a -> IO (Either ProcessorError a)
runProcessorM cfg act = do
  env <- ProcessorEnv <$> pure cfg <*> newIORef [] <*> newIORef []
  ret <- runProcessorEnv env act
  cleanup env `catch` (\(_::IOException) -> return ())
  return ret
  where
    cleanup env = do
      readIORef (pProcs env) >>= mapM_ killProc
      readIORef (pThreads env) >>= mapM_ (catchKillErr . Th.killSomeChild)
    killProc p = catchKillErr $ do
      mExitCode <- getProcessExitCode p
      case mExitCode of
        Nothing -> void $ forkIO $ do
                      terminateProcess p
                      threadDelay $ 10*1000000 -- give it 10 seconds to die
                      interruptProcessGroupOf p
        _       -> return ()
    catchKillErr = flip catch handleKillErr
    handleKillErr :: SomeException -> IO ()
    handleKillErr _ = return () -- TODO Log error


data ShellCmd
  = ShellCmd {
      shCmd   :: String
    , shInput :: Maybe (ProcessorSource ByteString)
    , shEnv   :: ShellEnv
  }

shellCmd :: String -> ShellCmd
shellCmd cmd = ShellCmd cmd Nothing mempty

createProcess
  :: CreateProcess
  -> ProcessorM (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  ret@(_,_,_,ph) <- liftIO $ P.createProcess cp
  pList <- ProcessorM (asks pProcs)
  liftIO $ atomicModifyIORef' pList (\ps -> (ph:ps,()))
  return ret

waitForProcess :: ProcessHandle -> ProcessorM ExitCode
waitForProcess = liftIO . P.waitForProcess
  
executeShellCmd :: ShellCmd -> ProcessorM (ByteString, ByteString)
executeShellCmd ShellCmd{..} = do
  env <- fmap (shellEnvToEnv . (`mappend` shEnv)) (asks pShellEnv)
  let process = (shell shCmd)
        { std_in       = maybe Inherit (const CreatePipe) shInput
        , std_out      = CreatePipe
        , std_err      = CreatePipe
        , env          = Just env
        , create_group = True
        }
  (exitCode, out, err) <- do
    p <- createProcess process
    case (p, shInput) of
      ((Just s_in, Just s_out, Just s_err, ph), Just input) -> do
        input $$ sinkHandle s_in
        exitCode <- waitForProcess ph
        liftIO $ do
          out <- BS.hGetContents s_out
          err <- BS.hGetContents s_err
          return (exitCode, out, err)
      ((_, Just s_out, Just s_err, ph), Nothing) -> do
        exitCode <- waitForProcess ph
        liftIO $ do
          out <- BS.hGetContents s_out
          err <- BS.hGetContents s_err
          return (exitCode, out, err)
      _ -> throwE $ InternalError "Unexpected state when creating process"
  case exitCode of
    ExitSuccess   -> return (out, err)
    ExitFailure c -> throwE (ShellError c out err)


shellProcessor :: [String] -> Processor
shellProcessor cmds filename content
  = let env  = envSet "FILENAME" filename mempty
        sh s = executeShellCmd $ (shellCmd s) {shInput=Just content, shEnv=env}
    in mapM_ sh cmds

forkChild :: ProcessorM a -> ProcessorM (ThreadHandle (Either ProcessorError a))
forkChild act = do
  env <- ProcessorM ask
  ret <- liftIO $ Th.forkChild (runProcessorEnv env act)
  tList <- ProcessorM (asks pThreads)
  liftIO $ atomicModifyIORef' tList (\ts -> (Th.toSomeThreadHandle ret:ts,()))
  return ret

killChild :: ThreadHandle a -> ProcessorM ()
killChild = liftIO . Th.killChild

waitChild :: ThreadHandle a -> ProcessorM a
waitChild = liftIO . Th.waitChild

tryWaitChild :: ThreadHandle a -> ProcessorM (Maybe a)
tryWaitChild = liftIO . Th.tryWaitChild

data ProcessorError
  = ProcessorException SomeException
  | ShellError {
      seCode   :: Int
    , seStdout :: ByteString
    , seStdErr :: ByteString
    }
  | InternalError String
  deriving (Show, Typeable)

instance Exception ProcessorError

throwE :: ProcessorError -> ProcessorM a
throwE = ProcessorM . E.throwE

catchE :: ProcessorM a -> (ProcessorError -> ProcessorM a) -> ProcessorM a
catchE act handler = do
  env <- ProcessorM ask
  either handler return =<< liftIO (runProcessorEnv env act)
