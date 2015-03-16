{-# LANGUAGE OverloadedStrings #-}
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
  , ToLogStr (..)
  , LogLevel (..)
  , LogSource
  , runLoggingT
  , LoggingT
  , logDebug
  , logInfo
  , logWarn
  , logError
  )
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as S8
import Data.Monoid(Monoid(..))
import System.IO (Handle, stderr)
import System.Log.FastLogger (LogStr, fromLogStr)
import Language.Haskell.TH.Syntax (Loc(..))
import Data.String (IsString(fromString))
import Data.Time (getCurrentTime)
import Control.Concurrent (myThreadId)

fromStrings :: IsString a => [String] -> a
fromStrings = fromString . concat

runStderrLoggingT :: MonadIO m => LoggingT m a -> m a
runStderrLoggingT = (`runLoggingT` defaultOutput stderr)

defaultOutput
  :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultOutput h loc _ level msg = do
    now <- getCurrentTime
    tid <- myThreadId
    S8.hPutStr h (format now tid)
  where
    format time tid = fromLogStr $ 
        fromStrings [show time, " [", loc_module loc, "] (", show tid, ") "]
        `mappend` "[" `mappend` defaultLogLevelStr level `mappend` "]: "
        `mappend` msg `mappend` "\n"

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level
