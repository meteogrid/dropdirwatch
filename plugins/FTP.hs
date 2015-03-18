{-# LANGUAGE OverloadedStrings #-}
module FTP where
import System.DirWatch.PluginAPI
import Network.FTP.Conduit (createSink)
import Network.URI (URI(..), parseURI)

newtype FTPUri = FTPUri URI

upload (FTPUri uri@URI{uriPath=destDir}) filename
  = ($$ createSink uri {uriPath=destDir ++ takeFileName filename})

instance FromJSON FTPUri where
  parseJSON (Object v) = do
    uri <- v .: "uri"
    case parseURI uri of
      Just (uri@URI{ uriScheme="ftp:"
                   , uriPath=path
                   , uriAuthority=Just _
                   , uriQuery=""
                   , uriFragment=""})
        | not (null path) && last path == '/'-> return (FTPUri uri)
      Just (uri@URI{ uriScheme="ftp:"
                   , uriPath=_
                   , uriAuthority=Just _
                   , uriQuery=""
                   , uriFragment=""})  -> fail "URI path have a trailing slash"
      Just (uri@URI{ uriScheme="ftp:"
                   , uriPath=_
                   , uriAuthority=_
                   , uriQuery=""
                   , uriFragment=""})  -> fail "URI must have a host"
      Just (uri@URI{ uriScheme=_
                   , uriPath=_
                   , uriAuthority=_
                   , uriQuery=""
                   , uriFragment=""})  -> fail "Expected a ftp URI"
      Just (uri@URI{ uriScheme=_
                   , uriPath=_
                   , uriAuthority=_
                   , uriQuery=_
                   , uriFragment=_})   -> fail "URI should not have query or fragment"
      Nothing                          -> fail "Could not parse URI"
