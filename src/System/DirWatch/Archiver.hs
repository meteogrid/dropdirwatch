{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module System.DirWatch.Archiver (
    Archiver (..)
  , archiveFilename
  , s3Archiver
) where

import           System.DirWatch.Logging (logInfo, logWarn)
import           System.DirWatch.Util (AbsPath, toFilePath , commonPrefix)

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Catch (SomeException, fromException, try)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson (
                   FromJSON (..)
                 , ToJSON (..)
                 , Value (..)
                 , object
                 , (.=)
                 , (.:)
                 , (.:?)
                 , (.!=)
                 )
import qualified Data.ByteString.Char8 as BS
import           Data.Fixed (Fixed, E2)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day, toGregorian)
import           Data.Time.Clock (UTCTime, getCurrentTime, utctDay, utctDayTime)
import           Network.HTTP.Conduit (tlsManagerSettings, newManager)
import           Network.HTTP.Client (streamFile)
import           System.IO.Error (tryIOError)
import           System.Directory  (createDirectoryIfMissing
                                  , removeFile
                                  , renameFile
                                  , doesFileExist
                                  )
import           System.FilePath.Posix (
                   takeDirectory
                 , takeFileName
                 , makeRelative
                 , joinPath
                 , (</>)
                 )
import qualified System.Log.Logger as Logger
import           Text.Printf (printf)


data Archiver
  = NoArchive
  | ArchiveDir AbsPath
  | ArchiveS3 {
      arS3KeyID          :: BS.ByteString
    , arS3AccessKey      :: BS.ByteString
    , arS3StripPrefix    :: AbsPath
    , arS3BucketPrefix   :: T.Text
    , arS3BucketLocation :: S3.LocationConstraint
    , arS3BucketAcl      :: S3.CannedAcl
    , arS3ObjectAcl      :: S3.CannedAcl
    , arS3ObjectStorage  :: S3.StorageClass
    }
  deriving Show

arS3Endpoint :: Archiver -> BS.ByteString
arS3Endpoint a
  = "s3-" <> T.encodeUtf8 (arS3BucketLocation a) <> ".amazonaws.com"

s3Archiver
  :: BS.ByteString
  -> BS.ByteString
  -> T.Text
  -> S3.LocationConstraint
  -> Archiver
s3Archiver keyId accessKey bucketPrefix location =
  ArchiveS3 {
    arS3KeyID          = keyId
  , arS3AccessKey      = accessKey
  , arS3StripPrefix    = "/"
  , arS3BucketPrefix   = bucketPrefix
  , arS3BucketLocation = location
  , arS3BucketAcl      = S3.AclPrivate
  , arS3ObjectAcl      = S3.AclPrivate
  , arS3ObjectStorage  = S3.Standard
  }


instance ToJSON Archiver where
  toJSON NoArchive      = Null
  toJSON (ArchiveDir s) = toJSON s
  toJSON ArchiveS3{..}  = object [
      "keyID"         .= T.decodeUtf8 arS3KeyID
    , "accessKey"     .= T.decodeUtf8 arS3AccessKey
    , "stripPrefix"   .= arS3StripPrefix
    , "bucketPrefix"  .= arS3BucketPrefix
    , "location"      .= arS3BucketLocation
    , "bucketAcl"     .= show arS3BucketAcl
    , "objectAcl"     .= show arS3ObjectAcl
    , "objectStorage" .= show arS3ObjectStorage
    ]

instance FromJSON Archiver where
  parseJSON Null       = return NoArchive
  parseJSON s@String{} = ArchiveDir <$> parseJSON s
  parseJSON (Object v) = do
    arS3KeyID          <- T.encodeUtf8 <$> v .: "keyID"
    arS3AccessKey      <- T.encodeUtf8 <$> v .: "accessKey"
    arS3StripPrefix    <- v .:? "stripPrefix" .!= "/"
    arS3BucketPrefix   <- v .: "bucketPrefix"
    arS3BucketLocation <- v .: "location"
    arS3BucketAcl      <- parseAcl =<< v .:? "bucketAcl"
    arS3ObjectAcl      <- parseAcl =<< v .:? "objectAcl"
    arS3ObjectStorage  <- parseStorage =<< v .:? "objectStorage"
    return ArchiveS3{..}
  parseJSON _          = fail "FromJSON(Archiver)"

parseAcl :: Monad m => Maybe T.Text -> m S3.CannedAcl
parseAcl Nothing = return (S3.AclPrivate)
parseAcl (Just "Private") = return (S3.AclPrivate)
parseAcl (Just "PublicRead") = return (S3.AclPublicRead)
parseAcl (Just "PublicReadWrite") = return (S3.AclPublicReadWrite)
parseAcl (Just "AuthenticatedRead") = return (S3.AclAuthenticatedRead)
parseAcl (Just "BucketOwnerRead") = return (S3.AclBucketOwnerRead)
parseAcl (Just "BucketOwnerFullControl") = return S3.AclBucketOwnerFullControl
parseAcl (Just "LogDeliveryWrite") = return (S3.AclLogDeliveryWrite)
parseAcl _                         = fail "invalid Acl"

parseStorage :: Monad m => Maybe T.Text -> m (S3.StorageClass)
parseStorage Nothing = return S3.Standard
parseStorage (Just "Standard") = return S3.Standard
parseStorage (Just "StandardInfrequentAccess") = return S3.StandardInfrequentAccess
parseStorage (Just "ReducedRedundancy") = return S3.ReducedRedundancy
parseStorage (Just "Glacier") = return S3.Glacier
parseStorage (Just other) = return (S3.OtherStorageClass other)


archiveFilename :: MonadIO m => Archiver -> Maybe UTCTime -> AbsPath -> m ()
archiveFilename NoArchive               _     _     = return ()

archiveFilename (ArchiveDir archiveDir) mTime fname = do
  time <- maybe (liftIO getCurrentTime) return mTime
  let dest = toFilePath archiveDir </> dPrefix </> dFilePath
      prefix = commonPrefix archiveDir fname
      (dPrefix, dFilePath) = archiveDestination prefix (utctDay time) fname
      destDir = takeDirectory dest
  exists <- liftIO $ doesFileExist dest
  let finalDest
        | exists    = dest ++ "." ++ show secs
        | otherwise = dest
      secs = realToFrac (utctDayTime time) :: Fixed E2
  result <- liftIO . tryIOError $ do
    createDirectoryIfMissing True destDir
    renameFile (toFilePath fname) finalDest
  case result of
    Left e -> do
      $(logWarn) $ concat [ "Could not archive ", show fname, ": "
                          , show e]
    Right () ->
      $(logInfo) $ concat ["Archived ", show fname, " -> ", finalDest]

archiveFilename ar@ArchiveS3{} mTime fname = do
  time <- maybe (liftIO getCurrentTime) return mTime
  creds <- Aws.makeCredentials (arS3KeyID ar) (arS3AccessKey ar)
  body <- liftIO (streamFile (toFilePath fname))
  let cfg = awsConfig creds
      bucket = arS3BucketPrefix ar <> goodPrefix
      badChars = ["/", "_", "."]
      goodPrefix = foldr (flip T.replace "-") (T.pack dPrefix) badChars
      (dPrefix, dFilePath) =
        archiveDestination (arS3StripPrefix ar) (utctDay time) fname
      key = T.pack dFilePath
      po = (S3.putObject bucket key body) {
              S3.poMetadata     = [("filename", T.pack (toFilePath fname))]
            , S3.poAcl          = Just (arS3ObjectAcl ar)
            , S3.poStorageClass = Just (arS3ObjectStorage ar)
            }
      pb = (S3.putBucket bucket) {
              S3.pbCannedAcl          = Just (arS3BucketAcl ar)
            , S3.pbLocationConstraint = arS3BucketLocation ar
            }
      s3cfg:: S3.S3Configuration Aws.NormalQuery
      s3cfg = S3.s3 Aws.HTTPS (arS3Endpoint ar) False
      location = concat ["https://", T.unpack bucket, ".s3.amazonaws.com/"
                        , dFilePath]
      success = logSuccess >> liftIO (removeFile (toFilePath fname))
      logSuccess = $(logInfo) $
        concat ["Archived ", show fname, " at ", location]
      logError e = $(logWarn) $
        concat ["Could not archive ", show fname, ": ", show e]
  void $ liftIO $ forkIO $ do
    mgr <- newManager tlsManagerSettings
    runResourceT $ do
      respPo <- try $ Aws.pureAws cfg s3cfg mgr po
      case respPo of
        Right _ -> success
        Left e | isNoBucketError e -> do
          $(logInfo) $ concat ["Creating bucket", show bucket]
          respPb <- try $ Aws.pureAws cfg s3cfg mgr pb
          case respPb of
            Left (ePb :: SomeException) -> $(logWarn) $
                concat ["Could not create bucket ", show bucket, ": ", show ePb]
            Right _ -> do
              respPo2 <- try $ Aws.pureAws cfg s3cfg mgr po
              case respPo2 of
                Right _ -> success
                Left (ePo2 :: SomeException) -> logError ePo2
        Left  e -> logError e

isNoBucketError :: SomeException -> Bool
isNoBucketError e
  | Just (S3.S3Error{S3.s3ErrorCode = "NoSuchBucket"}) <- fromException e
  = True
  | otherwise = False

-- |Calculates the filename where a file should be archived based on time
archiveDestination :: AbsPath -> Day -> AbsPath -> (FilePath, FilePath)
archiveDestination prefix utcDay filename
  = (dir, joinPath [padY y, padDM m, padDM d, fname])
  where (y,m,d) = toGregorian utcDay
        relDest = makeRelative (toFilePath prefix) (toFilePath filename)
        dir     = takeDirectory relDest
        fname   = takeFileName relDest
        padDM   = printf "%.2d"
        padY    = printf "%.4d"

awsConfig :: Aws.Credentials -> Aws.Configuration
awsConfig creds = Aws.Configuration {
    Aws.timeInfo    = Aws.Timestamp
  , Aws.credentials = creds
  , Aws.logger      = \lvl -> Logger.logM "aws" (fromAwsLevel lvl) . T.unpack
  }

fromAwsLevel :: Aws.LogLevel -> Logger.Priority
fromAwsLevel Aws.Debug   = Logger.DEBUG
fromAwsLevel Aws.Info    = Logger.INFO
fromAwsLevel Aws.Warning = Logger.WARNING
fromAwsLevel Aws.Error   = Logger.ERROR
