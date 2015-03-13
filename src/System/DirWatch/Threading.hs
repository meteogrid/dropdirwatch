module System.DirWatch.Threading (
    ThreadHandle
  , Timeout
  , forkChild
  , killChild
  , waitChild
) where

import System.Mem.Weak (Weak, deRefWeak)
import Control.Concurrent (
    ThreadId
  , killThread
  , forkFinally
  , threadDelay
  , mkWeakThreadId
  )
import Control.Concurrent.MVar (
    MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryTakeMVar
  )

type Timeout = Int
newtype ThreadHandle = ThreadHandle (Weak ThreadId, MVar ())

forkChild :: IO () -> IO ThreadHandle
forkChild io = do
   mvar <- newEmptyMVar
   tid <- forkFinally io (\_ -> putMVar mvar ()) >>= mkWeakThreadId
   return $ ThreadHandle (tid,mvar)

killChild :: ThreadHandle -> IO ()
killChild (ThreadHandle (tid,_))
  = maybe (return ()) killThread =<< deRefWeak tid

waitChild :: ThreadHandle -> Maybe Timeout -> IO ()
waitChild (ThreadHandle (_,mvar)) Nothing = takeMVar mvar
waitChild (ThreadHandle (_,mvar)) (Just t) = go t
  where
    delay = 1000
    go n | n<=0 = return ()
    go n = do
      tookIt <- tryTakeMVar mvar
      case tookIt of
        Nothing -> do
          threadDelay delay
          go (n-delay)
        Just () -> return ()
