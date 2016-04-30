module Main (main) where

import System.DirWatch.Archiver (migrateFromArchiveDir)
import System.DirWatch.Logging (Priority(..), setupLogging)
import System.DirWatch.Config (SerializableConfig, cfgArchiver)
import System.DirWatch.Util (mkAbsPath)
import Data.Yaml (ParseException, decodeFileEither)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cfgFile, aDir] -> do
      archiveDir <-
        maybe (fail "archiveDir must be an abs path") return (mkAbsPath aDir)
      eCfg <- decodeFileEither cfgFile
      case eCfg :: Either ParseException SerializableConfig of
        Left e -> fail ("Invalid config: " ++ show e)
        Right cfg -> do
          setupLogging ERROR INFO
          migrateFromArchiveDir (cfgArchiver cfg) archiveDir
    _ -> fail "Usage: dropdirwatch-migrateS3 cfgFile archiveDir"
