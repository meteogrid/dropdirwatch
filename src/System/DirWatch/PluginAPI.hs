{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    NoArgs
  , noArgs
  , modifyBaseName
  , toLazyByteStringC
  , toStrictByteStringC
  , formatTime
  , formattedCurrentTime
  , mkPlugin

  -- | Re-exports
  , MonadResource
  , liftIO
  , takeFileName
  , takeExtension
  , takeDirectory
  , joinPath
  , ($$)
  , (=$=)
  , sinkFile
  , sourceLbs
  , conduitMap
  , FromJSON (parseJSON)
  , Object
  , Value (Object)
  , (.:)
  , (.:?)
  , (.!=)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logCritical

  , module System.DirWatch.Processor
  , module System.DirWatch.PreProcessor
  , module System.DirWatch.ShellEnv
) where

import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Data.Aeson (
    FromJSON (..)
  , Object
  , Value (Object)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Conduit (Conduit, ($$), (=$=), await, yield)
import Data.Conduit.Binary (sinkFile, sourceLbs)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import System.FilePath.Posix
import System.DirWatch.Processor hiding (ProcessorConfig, runProcessorM)
import System.DirWatch.PreProcessor hiding (runPreProcessor)
import System.DirWatch.Types (HasCurrentTime(..))
import System.DirWatch.ShellEnv (envSet, envAppend)
import System.DirWatch.Logging (
  logDebug, logInfo, logWarn, logError, logCritical)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format(defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import qualified Data.Time.Format as F (FormatTime, formatTime)

conduitMap :: Monad m => (a -> b) -> Conduit a m b
conduitMap = CL.map
{-# INLINE conduitMap #-}

modifyBaseName :: (FilePath -> FilePath) -> FilePath -> FilePath
modifyBaseName func fpath = replaceBaseName fpath (func (takeBaseName fpath))

toLazyByteStringC
  :: MonadResource m => Conduit BS.ByteString m LBS.ByteString
toLazyByteStringC = go []
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
{-# INLINE toLazyByteStringC #-}
{-# INLINE toStrictByteStringC #-}

formatTime :: F.FormatTime t => String -> t -> String
formatTime = F.formatTime defaultTimeLocale

formattedCurrentTime :: HasCurrentTime m => String -> m String
formattedCurrentTime fmt = liftM (formatTime fmt) getTime

newtype NoArgs = NoArgs ()

instance FromJSON NoArgs where
  parseJSON (Object v)
    | HM.size v == 0 = return (NoArgs ())
    | otherwise      = fail "Expected no args for plugin"
  parseJSON _        = fail "Expected an objects as args"

noArgs :: NoArgs
noArgs = NoArgs ()

mkPlugin
  :: FromJSON a
  => (a -> b) -> Object -> Either String b
mkPlugin func config
  = case parseEither parseJSON (Object config) of
      Right v -> Right (func v)
      Left  e -> Left e
