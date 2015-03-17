{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    modifyBaseName
  , yieldModifiedBaseName
  , toLazyBytesStringC
  , toStrictByteStringC
  , formatTime
  , formattedCurrentTime
  , module API
) where

import Control.Applicative as API (pure, (<$>), (<*>), (<|>))
import Control.Monad.Trans.Resource as API (ResourceT)
import Data.Conduit as API
import Data.Conduit.Binary as API
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid as API (mempty, mappend, (<>))
import Data.Aeson as API (
    FromJSON (..)
  , Value (..)
  , (.:)
  , (.:?)
  , (.!=)
  )
import System.FilePath.Posix as API
import System.DirWatch.Processor as API hiding (ProcessorConfig, runProcessorM)
import System.DirWatch.PreProcessor as API hiding (runPreProcessor)
import System.DirWatch.ShellEnv as API (envSet, envAppend)

import System.Locale (defaultTimeLocale)
import qualified Data.Time.Format as F

modifyBaseName :: FilePath -> (FilePath -> FilePath) -> FilePath
modifyBaseName fpath func = replaceBaseName fpath (func (takeBaseName fpath))

yieldModifiedBaseName :: (FilePath -> FilePath) -> PreProcessor
yieldModifiedBaseName func = yieldFilePath . flip modifyBaseName func

toLazyBytesStringC :: ProcessorConduit BS.ByteString LBS.ByteString
toLazyBytesStringC = go []
  where
    go chunks = do
      mChunk <- await
      case mChunk of
        Nothing -> yield . LBS.fromChunks . reverse $ chunks
        Just chunk -> go (chunk:chunks)

toStrictByteStringC :: ProcessorConduit LBS.ByteString BS.ByteString
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

formattedCurrentTime :: String -> PreProcessorM String
formattedCurrentTime fmt = fmap (formatTime fmt) currentTime
