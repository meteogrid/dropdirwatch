{-# LANGUAGE OverloadedStrings #-}
module Zip where
import System.DirWatch.PluginAPI
import Codec.Archive.Zip (
    Archive(..)
  , Entry(..)
  , toArchive
  , fromArchive
  , fromEntry
  )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import System.FilePath.GlobPattern (GlobPattern, (~~))

newtype Prefix = Prefix String

instance FromJSON Prefix where
  parseJSON (Object v) = Prefix <$> v .: "prefix"

addPrefixToZipContents :: Prefix -> PreProcessor
addPrefixToZipContents (Prefix prefix) path = yieldConduit path conduit
  where
    conduit
        = toLazyBytesStringC
      =$= CL.map (fromArchive . modifyArchive . toArchive)
      =$= toStrictByteStringC
    modifyArchive a@Archive{zEntries=es} = a {zEntries=map modifyEntry es}
    modifyEntry e@Entry{eRelativePath=relPath} = e {eRelativePath = newPath}
      where newPath = modifyBaseName relPath (prefix++)

newtype GlobPatterns = GlobPatterns (Maybe [GlobPattern])
instance FromJSON GlobPatterns where
  parseJSON (Object v) = GlobPatterns <$> v .:? "patterns"

extractZip :: GlobPatterns -> PreProcessor
extractZip (GlobPatterns mPatterns) _ = do
  (mapM_ yieldEntry . filter matches . zEntries . toArchive =<< getLbs)
  where
    yieldEntry e = yieldSource (eRelativePath e) (CB.sourceLbs (fromEntry e))
    matches e = case mPatterns of
                  Just patterns -> any (eRelativePath e ~~) patterns
                  Nothing       -> True
