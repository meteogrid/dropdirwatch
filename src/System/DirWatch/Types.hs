module System.DirWatch.Types (HasCurrentTime(..)) where

import Data.Time (UTCTime)

class Monad m => HasCurrentTime m where
  getTime :: m UTCTime
