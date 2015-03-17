{-# LANGUAGE OverloadedStrings #-}
module Zip where
import System.DirWatch.PluginAPI
import Codec.Archive.Zip (Archive(..), Entry(..), toArchive , fromArchive)
import qualified Data.Conduit.List as CL
import Debug.Trace (traceShow)

newtype Prefix = Prefix String

instance FromJSON Prefix where
  parseJSON (Object v) = Prefix <$> v .: "prefix"

addPrefixToZipContents (Prefix prefix) path = yieldConduit path conduit
  where
    conduit
        = toLazyBytesStringC
      =$= CL.map (fromArchive . modifyArchive . toArchive)
      =$= toStrictByteStringC

    modifyArchive a@Archive{zEntries=entries}
      = a {zEntries=map modifyEntry entries}

    modifyEntry e@Entry{eRelativePath=relPath}
      = e {eRelativePath = traceShow (relPath, newPath) newPath}
      where newPath = modifyBaseName relPath (prefix++)
