module System.DirWatch.Util (patternDir) where

import System.FilePath.Posix (joinPath, splitPath)
import System.FilePath.GlobPattern (GlobPattern)

patternDir :: GlobPattern -> FilePath
patternDir = joinPath . takeWhile notWildcard . splitPath
  where
    notWildcard p = all (`notElem` p) "?*[]()|"
