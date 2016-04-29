{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.DirWatch.Util (
    AbsPath
  , mkAbsPath
  , joinAbsPath
  , globMatch
  , absPathsMatching
  , absPathFilename
  , toFilePath
  , takePatternDirectory
  , commonPrefix
  ) where

import Data.Aeson (FromJSON (..) , ToJSON (..) , Value (String))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import System.FilePath.Posix (
    joinPath
  , splitPath
  , makeRelative
  , takeDirectory
  , takeFileName
  , normalise
  , isAbsolute
  )
import Data.String (IsString(..))
import System.FilePath.Glob (namesMatching)
import System.FilePath.GlobPattern ((~~))

-- |Takes the non-wildcard deepest directory name from a GlobPattern
takePatternDirectory :: AbsPath -> AbsPath
takePatternDirectory
  = AbsPath . takeDirectory . joinPath . takeWhile notWildcard . splitPath
  . unAbsPath
  where
    notWildcard p = all (`notElem` p) "?*[]()|"


commonPrefix :: AbsPath -> AbsPath -> AbsPath
commonPrefix (AbsPath a) (AbsPath b)
  = AbsPath . joinPath . map fst . takeWhile (uncurry (==))
  $ zip (splitPath a) (splitPath b)


newtype AbsPath = AbsPath {unAbsPath::FilePath} deriving (Eq, Hashable)
instance Show AbsPath where show (AbsPath p) = show p

instance IsString AbsPath where
  fromString s = fromMaybe (error ("Invalid absolute path literal: " ++ s))
               $ mkAbsPath s

mkAbsPath :: FilePath -> Maybe AbsPath
mkAbsPath p
  | isAbsolute p' = Just (AbsPath p')
  | otherwise     = Nothing
  where p' = normalise p
{-# INLINE mkAbsPath #-}

joinAbsPath :: AbsPath -> [FilePath] -> AbsPath
joinAbsPath (AbsPath base) = AbsPath . joinPath . (base:)
{-# INLINE joinAbsPath #-}


globMatch :: AbsPath -> AbsPath -> Bool
globMatch (AbsPath p) (AbsPath g) = p ~~ g && matchVisibility fp fg
  where
    matchVisibility ('.':a) ('.':b) = a ~~ b
    matchVisibility ('.':_) _       = False
    matchVisibility a       b       = a ~~ b
    fp = takeFileName p
    fg = takeFileName g
{-# INLINE globMatch #-}

absPathsMatching :: AbsPath -> IO [AbsPath]
absPathsMatching p@(AbsPath pattern)
  = fmap (filter (`globMatch` p) . map AbsPath) (namesMatching pattern)
{-# INLINE absPathsMatching #-}


toFilePath :: AbsPath -> FilePath
toFilePath (AbsPath p) = p
{-# INLINE toFilePath #-}

absPathFilename :: AbsPath -> FilePath
absPathFilename (AbsPath p) = takeFileName p
{-# INLINE absPathFilename #-}

instance ToJSON AbsPath where
  toJSON = toJSON . unAbsPath
instance FromJSON AbsPath where
  parseJSON (String s)
    = let s' = unpack s
      in maybe (fail $"Not an absolute path: " ++ s') return (mkAbsPath s')
  parseJSON _ = fail "FromJSON(AbsPath): Expected a string"
