{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    modifyBaseName
  , yieldFileName
  , toLazyBytesStringC
  , toStrictByteStringC
  , formatTime
  , formattedCurrentTime
  , module API
) where

import Control.Applicative as API (pure, (<$>), (<*>), (<|>))
import Control.Monad.Trans.Resource as API (MonadResource)
import Control.Monad (liftM)
import Data.Monoid as API (mempty, mappend, (<>))
import Data.Aeson as API (
    FromJSON (..)
  , Value (..)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Conduit as API (Conduit, Source, (=$=), ($$), await, yield)
import System.FilePath.Posix as API
import System.DirWatch.Processor as API hiding (ProcessorConfig, runProcessorM)
import System.DirWatch.PreProcessor as API hiding (runPreProcessor)
import System.DirWatch.ShellEnv as API (envSet, envAppend)
import Data.ByteString as API (ByteString)

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
