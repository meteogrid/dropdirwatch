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
  , getWorkSpace
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
  , def
) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (IOException, fromException)
import Control.Exception.Lifted as E (
    Exception
  , SomeException
  , handle
  , catch
  , finally
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
import Data.Time (UTCTime)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default(def))
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Monoid (mempty, mappend)
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOErrorType (ResourceVanished))
import System.DirWatch.Logging (
    MonadLogger
  , LoggingT
  , runStderrLoggingT
  , logError
  , fromStrings
  )
import System.DirWatch.Types (HasCurrentTime(..))
import System.DirWatch.ShellEnv
import System.DirWatch.Threading (ThreadHandle, SomeThreadHandle)
import qualified System.DirWatch.Threading as Th
import System.Exit (ExitCode(..))
import System.IO.Error (ioeGetErrorType)
import System.IO.Temp (withSystemTempDirectory)
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
type ProcessorSource = Source ProcessorM ByteString

type Processor = FilePath -> ProcessorSource -> ProcessorM ()

data ProcessorConfig
  = ProcessorConfig {
      pShellEnv :: ShellEnv
  }

data ProcessorEnv
  = ProcessorEnv {
      pConfig    :: ProcessorConfig
    , pProcs     :: IORef [ProcessHandle]
    , pThreads   :: IORef [SomeThreadHandle ProcessorError]
    , pWorkSpace :: FilePath
    , pCurTime   :: UTCTime
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
    
instance HasCurrentTime ProcessorM where
  getTime = ProcessorM (asks pCurTime)

instance MonadBaseControl IO ProcessorM where
   type StM ProcessorM a = Either ProcessorError a
   liftBaseWith f = do
     env <- ProcessorM ask
     liftIO $ f (runProcessorEnv env)
   restoreM = either throwE return

runProcessorM
  :: ProcessorConfig -> UTCTime -> ProcessorM a -> IO (Either ProcessorError a)
runProcessorM cfg time act
  = withSystemTempDirectory ".dropdirwatch-processor" $ \workSpace -> do
      env <- ProcessorEnv <$> pure cfg <*> newIORef [] <*> newIORef []
                          <*> pure workSpace <*> pure time
      finally (runProcessorEnv env act) (cleanup env)
  where
    cleanup env = finally
      (readIORef (pThreads env) >>= mapM_ killThread)
      (readIORef (pProcs env) >>= mapM_ killProc)
    killProc p = flip catch handleProcKillErr $ do
      mExitCode <- getProcessExitCode p
      case mExitCode of
        Nothing -> void $ forkIO $ (do
                      terminateProcess p
                      threadDelay $ 10*1000000 -- give it 10 seconds to TERM...
                      interruptProcessGroupOf p -- ... or kill it
                      ) `catch` handleProcKillErr
        _       -> return ()
    killThread th = flip catch (handleThreadKillErr th) . Th.killSomeChild $ th
    logIt = runStderrLoggingT . $(logError) . fromStrings
    handleThreadKillErr th (e :: IOException)
      = logIt ["Error when killing unfinished thread ", show th, ": ", show e]
    handleProcKillErr (e :: IOException)
      = logIt ["Error when killing unfinished procress: ", show e]


data ShellCmd
  = ShellCmd {
      shCmd   :: String
    , shInput :: Maybe ProcessorSource
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
        (input $$ sinkHandle s_in) `catchE` ignoreResourceVanished
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
    ExitFailure c -> throwE (ShellError shCmd c out err)

ignoreResourceVanished :: ProcessorError -> ProcessorM ()
ignoreResourceVanished pe@(ProcessorException e) =
  case fromException e of
    -- Command closed pipe without reading from it, ignore it
    Just ioe | ioeGetErrorType ioe == ResourceVanished -> return ()
    _                                                  -> throwE pe
ignoreResourceVanished e = throwE e

-- | Execute several shell commands.
--   NOTE: Only the first command will get the contents piped into it
shellProcessor :: [String] -> Processor
shellProcessor [] _ _ = return ()
shellProcessor (x:xs) filename content = do
  workSpace <- getWorkSpace
  let env  = envSet "FILENAME" filename
           . envSet "WORKSPACE" workSpace
           $ mempty
  void $ executeShellCmd $ (shellCmd x) {shInput=Just content, shEnv=env}
  mapM_ (\s -> executeShellCmd $ (shellCmd s) {shEnv=env}) xs

getWorkSpace :: ProcessorM FilePath
getWorkSpace = ProcessorM (asks pWorkSpace)

forkChild
  :: ProcessorM a -> ProcessorM (ThreadHandle (Either ProcessorError a))
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
      seCmd    :: String
    , seCode   :: Int
    , seStdOut :: ByteString
    , seStdErr :: ByteString
    }
  | InternalError String
  deriving (Typeable)

instance Exception ProcessorError
instance Show ProcessorError where
  show ShellError{..}
    = concat $ [ "ShellError: Command ", show seCmd, " exited with status "
               , show seCode, ".\n"] ++
               (if BS.null seStdOut
                 then []
                 else ["Captured stdout:\n", tryDecode seStdOut, "\n"]) ++
               (if BS.null seStdErr
                 then [] else ["Captured stderr:\n", tryDecode seStdErr, "\n"])
  show (ProcessorException e) = concat ["ProcessorException: ", show e]
  show (InternalError e) = concat ["InternalError: ", e]

tryDecode :: ByteString -> String
tryDecode s = either (const (show s)) T.unpack (decodeUtf8' s)

throwE :: ProcessorError -> ProcessorM a
throwE = ProcessorM . E.throwE

catchE :: ProcessorM a -> (ProcessorError -> ProcessorM a) -> ProcessorM a
catchE act handler = do
  env <- ProcessorM ask
  either handler return =<< liftIO (runProcessorEnv env act)
