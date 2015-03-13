module System.DirWatch.Logging (
    MonadLogger
  , LoggingT
  , runStderrLoggingT
  , logDebug
  , logInfo
  , logWarn
  , logError
  , fromStrings
) where

import Control.Monad.Logger (
    MonadLogger
  , LoggingT
  , runStderrLoggingT
  , logDebug
  , logInfo
  , logWarn
  , logError
  )
import qualified Data.Text as T
import Data.String (IsString(fromString))

fromStrings :: [String] -> T.Text
fromStrings = T.concat . map T.pack
