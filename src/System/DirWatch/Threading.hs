{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , tryWaitSomeChild
) where

import Control.Exception (throw)
import Control.Concurrent (
    ThreadId
  , killThread
  , forkFinally
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
newtype ThreadHandle a = ThreadHandle (ThreadId, MVar a)
instance Show (ThreadHandle a) where
  show (ThreadHandle (tid,_)) = concat ["ThreadHandle(", show tid, ")"]
instance Eq (ThreadHandle a) where
  ThreadHandle (tid1,_) == ThreadHandle (tid2,_) = tid1 == tid2
  

forkChild :: IO a -> IO (ThreadHandle a)
forkChild io = do
   mvar <- newEmptyMVar
   tid <- forkFinally io (putMVar mvar . either throw id)
   return $ ThreadHandle (tid,mvar)

killChild :: ThreadHandle a -> IO ()
killChild (ThreadHandle (tid,_)) = killThread tid

waitChild :: ThreadHandle a -> IO a
waitChild (ThreadHandle (_,mvar)) = takeMVar mvar

tryWaitChild  :: ThreadHandle a -> IO (Maybe a)
tryWaitChild (ThreadHandle (_,mvar)) = tryTakeMVar mvar

tryWaitSomeChild  :: SomeThreadHandle e -> IO (Maybe (Maybe e))
tryWaitSomeChild (SomeTH th) = do
  r <- tryWaitChild th
  return $ case r of
    Nothing        -> Nothing
    Just (Right _) -> Just Nothing
    Just (Left e)  -> Just (Just e)

data SomeThreadHandle e = forall a. SomeTH (ThreadHandle (Either e a))
deriving instance Show (SomeThreadHandle e)
instance Eq (SomeThreadHandle e) where
  SomeTH (ThreadHandle (tid1,_)) == SomeTH (ThreadHandle (tid2,_))
    = tid1 == tid2

toSomeThreadHandle
  :: ThreadHandle (Either e a) 
  -> SomeThreadHandle e
toSomeThreadHandle = SomeTH . unsafeCoerce

killSomeChild :: SomeThreadHandle e -> IO ()
killSomeChild (SomeTH th) = killChild th
