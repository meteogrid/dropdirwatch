{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    NoArgs
  , noArgs
  , modifyBaseName
  , yieldFileName
  , toLazyBytesStringC
  , toStrictByteStringC
  , formatTime
  , formattedCurrentTime
  , module System.DirWatch.Processor
  , module System.DirWatch.PreProcessor
  , module System.DirWatch.ShellEnv
  , module System.DirWatch.Logging
  , module System.FilePath.Posix
  , module Control.Applicative
  , module Control.Monad.Trans.Resource
  , module Data.Aeson
  , module Data.ByteString
  , module Data.Conduit
  , module Data.Conduit.Binary
  , module Data.Monoid
) where

import Control.Applicative (pure, (<$>), (<*>), (<|>))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad (liftM)
import Data.Monoid (mempty, mappend, (<>))
import Data.Aeson (
    FromJSON (..)
  , Value (..)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Conduit (Conduit, Source, (=$=), ($$), await, yield)
import Data.Conduit.Binary (sourceLbs, sinkLbs)
import qualified Data.HashMap.Strict as HM
import System.FilePath.Posix
import System.DirWatch.Processor hiding (ProcessorConfig, runProcessorM)
import System.DirWatch.PreProcessor hiding (runPreProcessor)
import System.DirWatch.ShellEnv (envSet, envAppend)
import System.DirWatch.Logging (logDebug, logInfo, logWarn, logError)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import qualified Data.Time.Format as F (FormatTime, formatTime)

modifyBaseName :: (FilePath -> FilePath) -> FilePath -> FilePath
modifyBaseName func fpath = replaceBaseName fpath (func (takeBaseName fpath))

yieldFileName  :: Monad m => PreProcessor m
yieldFileName = yieldFilePath . takeFileName

toLazyBytesStringC
  :: MonadResource m => Conduit BS.ByteString m LBS.ByteString
toLazyBytesStringC = go []
  where
    go chunks = do
      mChunk <- await
      case mChunk of
        Nothing -> yield . LBS.fromChunks . reverse $ chunks
        Just chunk -> go (chunk:chunks)

toStrictByteStringC
  :: MonadResource m => Conduit LBS.ByteString m BS.ByteString
toStrictByteStringC = go []
  where
    go chunks = do
      mChunk <- await
      case mChunk of
        Nothing -> yield . BS.concat . map LBS.toStrict . reverse $ chunks
        Just chunk -> go (chunk:chunks)
{-# INLINE toLazyBytesStringC #-}
{-# INLINE toStrictByteStringC #-}

formatTime :: F.FormatTime t => String -> t -> String
formatTime = F.formatTime defaultTimeLocale

formattedCurrentTime :: Monad m => String -> PreProcessorT m String
formattedCurrentTime fmt = liftM (formatTime fmt) getTime

newtype NoArgs = NoArgs ()

instance FromJSON NoArgs where
  parseJSON (Object v)
    | HM.size v == 0 = return (NoArgs ())
    | otherwise      = fail "Expected no args for plugin"
  parseJSON _        = fail "Expected an objects as args"

noArgs :: NoArgs
noArgs = NoArgs ()
