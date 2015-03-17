{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    modifyBaseName
  , toLazyBytesStringC
  , toStrictByteStringC
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
import System.DirWatch.Processor as API hiding (ProcessorConfig)
import System.DirWatch.ShellEnv as API (envSet, envAppend)

modifyBaseName :: FilePath -> (FilePath -> FilePath) -> FilePath
modifyBaseName fpath func = replaceBaseName fpath (func (takeBaseName fpath))

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
