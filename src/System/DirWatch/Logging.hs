{-# LANGUAGE TemplateHaskell #-}

module System.DirWatch.Logging (
    Priority (..)
  , logM
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logCritical
  , setupLogging
) where

import System.Log.Logger (
    updateGlobalLogger, rootLoggerName, removeAllHandlers, addHandler, setLevel
  , Priority(..))
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler.Syslog (openlog, Facility(DAEMON))
import System.Log.Formatter (simpleLogFormatter)
import System.IO (stderr)
import qualified System.Log.Logger as Logger (logM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)

logTH :: Priority -> Q Exp
logTH prio =
    [|logMLoc $(qLocation >>= liftLoc) $(lift prio) . (id :: String -> String)|]


logDebug, logInfo, logWarn, logError, logCritical :: Q Exp

logDebug    = logTH DEBUG
logInfo     = logTH INFO
logWarn     = logTH WARNING
logError    = logTH ERROR
logCritical = logTH CRITICAL


logM :: MonadIO m => String -> Priority -> String -> m ()
logM fac prio = liftIO . Logger.logM fac prio

logMLoc :: MonadIO m => Loc -> Priority -> String -> m ()
logMLoc loc prio = logM (loc_module loc) prio

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]

instance Lift Priority where
    lift DEBUG     = [|DEBUG|]
    lift INFO      = [|INFO|]
    lift NOTICE    = [|NOTICE|]
    lift WARNING   = [|WARNING|]
    lift ERROR     = [|ERROR|]
    lift CRITICAL  = [|CRITICAL|]
    lift ALERT     = [|ALERT|]
    lift EMERGENCY = [|EMERGENCY|]

setupLogging :: Priority -> Priority -> IO ()
setupLogging syslogPrio stderrPrio = do
  let stderrFmt = simpleLogFormatter "$time [$loggername] ($tid) [$prio]: $msg" 
      syslogFmt = simpleLogFormatter "[$prio]: $msg" 
  syslogHandler <- fmap (flip setFormatter syslogFmt) $
                     openlog "dropdirwatch" [] DAEMON syslogPrio
  stderrHandler <- fmap (flip setFormatter stderrFmt) $
                     streamHandler stderr stderrPrio
  removeAllHandlers
  updateGlobalLogger rootLoggerName
    (setLevel DEBUG . addHandler syslogHandler . addHandler stderrHandler)
