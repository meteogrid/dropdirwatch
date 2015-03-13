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
) where

import Control.Exception (throw)
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

waitChild :: ThreadHandle a -> Maybe Timeout -> IO (Maybe a)
waitChild (ThreadHandle (_,mvar)) Nothing = fmap Just (takeMVar mvar)
waitChild (ThreadHandle (_,mvar)) (Just t) = go t
  where
    delay = 1000
    go n | n<=0 = return Nothing
    go n = do
      tookIt <- tryTakeMVar mvar
      case tookIt of
        Nothing -> do
          threadDelay delay
          go (n-delay)
        Just a -> return (Just a)

data SomeThreadHandle e = forall a. SomeTH (ThreadHandle (Either e a))

toSomeThreadHandle
  :: ThreadHandle (Either e a) 
  -> SomeThreadHandle e
toSomeThreadHandle = SomeTH . unsafeCoerce

killSomeChild :: SomeThreadHandle e -> IO ()
killSomeChild (SomeTH th) = killChild th
