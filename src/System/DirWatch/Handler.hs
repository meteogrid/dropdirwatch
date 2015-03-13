{-# LANGUAGE DeriveDataTypeable #-}
module System.DirWatch.Handler (
    Handler (..)
) where

import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)

newtype Handler
  = Handler {
      handle :: FilePath -> LBS.ByteString -> IO ()
  } deriving Typeable

{-
newtype HandlerM a
  = HandlerM {
      unHandlerM :: ExceptT SomeException (LoggingT (ReaderT Settings IO )) a
      }
  deriving ( Functor, Applicative, Monad, MonadLogger, MonadReader Settings
           , MonadIO)
-}
