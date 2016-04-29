{-# LANGUAGE TemplateHaskell #-}

module System.DirWatch.Archiver (
    Archiver (..)
  , archiveFilename
) where

import System.DirWatch.Logging (logInfo, logWarn)
import System.DirWatch.Util (
    AbsPath
  , archiveDestination
  , toFilePath
  )
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (
    FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , object
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  )
import Data.Fixed (Fixed, E2)
import Data.Time.Clock (getCurrentTime, utctDay, utctDayTime)
import System.IO.Error (tryIOError)
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)
import System.FilePath.Posix (takeDirectory)



data Archiver
  = NoArchive
  | ArchiveDir AbsPath
  deriving Show

instance ToJSON Archiver where
  toJSON NoArchive      = Null
  toJSON (ArchiveDir s) = toJSON s

instance FromJSON Archiver where
  parseJSON Null       = return NoArchive
  parseJSON s@String{} = ArchiveDir <$> parseJSON s
  parseJSON _          = fail "FromJSON(Archiver)"

archiveFilename :: MonadIO m => Archiver -> AbsPath -> m ()
archiveFilename NoArchive               _     = return ()
archiveFilename (ArchiveDir archiveDir) fname = do
  time <- liftIO getCurrentTime
  let dest = toFilePath
               (archiveDestination archiveDir (utctDay time) fname)
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
