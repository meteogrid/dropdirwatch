{-# LANGUAGE OverloadedStrings #-}
module Plugin where
import System.DirWatch.PluginAPI

newtype Prefix = Prefix String

instance FromJSON Prefix where
  parseJSON (Object v) = Prefix <$> v .: "prefix"

addPrefix (Prefix prefix) path contents
  = [(replaceBaseName path prefix, contents)]
