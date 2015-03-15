module System.DirWatch.Util (takePatternDirectory, archiveDestination, commonPrefix) where

import System.FilePath.Posix (
    joinPath
  , splitPath
  , makeRelative
  , takeDirectory
  , takeFileName
  )
import System.FilePath.GlobPattern (GlobPattern)
import Data.Time.Calendar (Day, toGregorian)

-- |Takes the non-wildcard deepest directory name from a GlobPattern
takePatternDirectory :: GlobPattern -> FilePath
takePatternDirectory = joinPath . takeWhile notWildcard . splitPath
  where
    notWildcard p = all (`notElem` p) "?*[]()|"

-- |Calculates the filename where a file should be archived based on time
archiveDestination :: FilePath -> Day -> FilePath -> FilePath
archiveDestination archiveDir utcDay filename
  = joinPath [archiveDir, dir, show y, show m, show d, fname]
  where common  = commonPrefix archiveDir filename
        (y,m,d) = toGregorian utcDay
        relDest = makeRelative common filename
        dir     = takeDirectory relDest
        fname   = takeFileName relDest

commonPrefix :: FilePath -> FilePath -> FilePath
commonPrefix a b
  = joinPath . map fst . takeWhile (uncurry (==)) $ zip (splitPath a) (splitPath b)
