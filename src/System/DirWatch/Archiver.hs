{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module System.DirWatch.Archiver (
    Archiver (..)
  , archiveFilename
  , s3Archiver
  , migrateFromArchiveDir
) where

import           System.DirWatch.Logging (logInfo, logWarn)
import           System.DirWatch.Util (
                   AbsPath
                 , mkAbsPath
                 , toFilePath
                 , commonPrefix
                 )

import qualified Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import           Control.Monad (void, when, liftM, forM_, foldM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Catch (SomeException, fromException, try, bracket_)
import           Control.Monad.Trans.Resource (runResourceT)
import           Crypto.Hash.Algorithms
import           Crypto.Hash (Digest, hashlazy)
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
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit (await, ($$))
import           Data.Conduit.Combinators (sourceDirectoryDeep)
import           Data.Fixed (Fixed, E2)
import           Data.List (intercalate, sort)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day, toGregorian)
import           Data.Time.Clock (UTCTime, getCurrentTime, utctDay, utctDayTime)
import           Network.HTTP.Conduit (tlsManagerSettings, newManager)
import           Network.HTTP.Client (Manager, streamFile)
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
                 , splitPath
                 , dropTrailingPathSeparator
                 , (</>)
                 )
import qualified System.Log.Logger as Logger
import           Text.Printf (printf)
import           Text.Read (readMaybe)


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
  let (dPrefix, dFilePath) =
        archiveDestination (arS3StripPrefix ar) (utctDay time) fname
  void $ liftIO $ forkIO $ do
    mgr <- newManager tlsManagerSettings
    uploadToS3 mgr ar dPrefix dFilePath fname

uploadToS3 :: Manager -> Archiver -> String -> String -> AbsPath -> IO ()
uploadToS3 mgr ar@ArchiveS3{} dPrefix dFilePath fname
  = tryUpload 10
  where
    retryDelay = 10000000 :: Int
    tryUpload 0 = return ()
    tryUpload n = do
      creds <- Aws.makeCredentials (arS3KeyID ar) (arS3AccessKey ar)
      body <- streamFile (toFilePath fname)
      let key = T.pack dFilePath
          cfg = awsConfig creds
          bucket = T.toLower (arS3BucketPrefix ar <> goodPrefix)
          badChars = ["/", "_", "."]
          goodPrefix = foldr (flip T.replace "-") (T.pack dPrefix) badChars
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
          onSuccess = logSuccess >> liftIO (removeFile (toFilePath fname))
          onError e = logError e >> threadDelay retryDelay >> tryUpload (n-1)

          logSuccess = $(logInfo) $
            concat ["Archived ", show fname, " at ", location]
          logError e = $(logWarn) $
            concat [ "Could not archive ", show fname, ": ", show e
                   , ". Will retry ", show (n-1), " more times" ]
      respPo <- runResourceT $ try $ Aws.pureAws cfg s3cfg mgr po
      case respPo of
        Right _ -> onSuccess
        Left e | isNoBucketError e -> do
          $(logInfo) $ concat ["Creating bucket", show bucket]
          respPb <- runResourceT $ try $ Aws.pureAws cfg s3cfg mgr pb
          case respPb of
            Left (ePb :: SomeException) -> $(logWarn) $
                concat ["Could not create bucket ", show bucket, ": ", show ePb]
            Right _ -> do
              respPo2 <- runResourceT $ try $ Aws.pureAws cfg s3cfg mgr po
              case respPo2 of
                Right _ -> onSuccess
                Left (ePo2 :: SomeException) -> onError ePo2
        Left  e -> onError e
uploadToS3 _ _ _ _ _ = fail "Need a ArchiveS3"

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

migrateFromArchiveDir :: Archiver -> AbsPath -> IO ()
migrateFromArchiveDir ar (toFilePath -> archiveDir) = do
  mgr <- newManager tlsManagerSettings
  sem <- newQSem 5
  runResourceT $
    sourceDirectoryDeep False archiveDir $$ loop sem mgr "" HM.empty
  where
    loop sem mgr prevDir !prevDupes = do
      mPath <- await
      case mPath of
        Nothing -> return ()
        Just realPath -> do
          let inNewDir = curDir /= prevDir
              curDupes = if inNewDir then HM.empty else prevDupes
              curDir   = takeDirectory realPath
          when (inNewDir && not (HM.null prevDupes)) $
            processDupes sem mgr prevDupes
          let (path, rev) = splitRevision realPath
              newDupes = HM.insertWith (++) path [(rev,realPath)] curDupes
          loop sem mgr curDir newDupes

    md5sum :: FilePath -> IO (Digest MD5)
    md5sum = liftM hashlazy . LBS.readFile

    stepCheckSums m (r,p) = do
      csum <- md5sum p
      return (M.insert csum (r,p) m)

    processDupes sem mgr dupes =
      forM_ (HM.toList dupes) $ \(path, revPaths) -> do
      realPaths <-
        if length revPaths == 1 then return (map snd revPaths) else do
          $(logInfo) "Processing dupes"
          revPaths2 <- liftIO $ foldM stepCheckSums M.empty revPaths
          return . map snd . sort . M.elems $ revPaths2
      liftIO $ mapM_ (uploadIt sem mgr path) realPaths
      when (length realPaths > 1) ($(logInfo) "End dupes")


    uploadIt s mgr path realPath = bracket_ (waitQSem s) (signalQSem s) $ do
      let (fps, pps) = splitAt 4 . reverse . splitPath $ path
          dPrefix = dropTrailingPathSeparator
                  . makeRelative archiveDir
                  . joinPath
                  . reverse
                  $ pps
          dFilePath = joinPath (reverse fps)
          Just fname = mkAbsPath realPath
      -- $(logInfo) (show (fname, dPrefix, dFilePath))
      uploadToS3 mgr ar dPrefix dFilePath fname

splitRevision :: FilePath -> (FilePath, Double)
splitRevision path = (fname, fromMaybe 0 rev)
  where
    (rParts, fParts) = splitAt 2 . reverse . splitOn "." . takeFileName $ path
    rev = readMaybe (intercalate "." (reverse rParts))
    fname  = case rev of
               Just _ -> takeDirectory path </> intercalate "." (reverse fParts)
               Nothing -> path
