module System.DirWatch.Util (enumerate, patternDir) where

import System.FilePath.Posix (joinPath, splitPath)
import System.FilePath.GlobPattern (GlobPattern)

enumerate :: Int -> [a] -> [(Int,a)]
enumerate x0 = zip [x0..]

          
patternDir :: GlobPattern -> FilePath
patternDir = joinPath . takeWhile notWildcard . splitPath
  where
    notWildcard p = all (`notElem` p) "?*[]()|"
