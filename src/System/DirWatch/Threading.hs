{-# LANGUAGE ExistentialQuantification #-}
module System.DirWatch.Threading (
    ThreadHandle
  , SomeThreadHandle
  , toSomeThreadHandle
  , Timeout
  , forkChild
  , killChild
  , killSomeChild
  , waitChild
  , tryWaitChild
) where

import Control.Exception (throw)
import System.Mem.Weak (Weak, deRefWeak)
import Control.Concurrent (
    ThreadId
  , killThread
  , forkFinally
  , mkWeakThreadId
  )
import Control.Concurrent.MVar (
    MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryTakeMVar
  )
import Unsafe.Coerce (unsafeCoerce)

type Timeout = Int
newtype ThreadHandle a = ThreadHandle (Weak ThreadId, MVar a)

forkChild :: IO a -> IO (ThreadHandle a)
forkChild io = do
   mvar <- newEmptyMVar
   tid <- forkFinally io (putMVar mvar . either throw id) >>= mkWeakThreadId
   return $ ThreadHandle (tid,mvar)

killChild :: ThreadHandle a -> IO ()
killChild (ThreadHandle (tid,_))
  = maybe (return ()) killThread =<< deRefWeak tid

waitChild :: ThreadHandle a -> IO a
waitChild (ThreadHandle (_,mvar)) = takeMVar mvar

tryWaitChild  :: ThreadHandle a -> IO (Maybe a)
tryWaitChild (ThreadHandle (_,mvar)) = tryTakeMVar mvar

data SomeThreadHandle e = forall a. SomeTH (ThreadHandle (Either e a))

toSomeThreadHandle
  :: ThreadHandle (Either e a) 
  -> SomeThreadHandle e
toSomeThreadHandle = SomeTH . unsafeCoerce

killSomeChild :: SomeThreadHandle e -> IO ()
killSomeChild (SomeTH th) = killChild th
