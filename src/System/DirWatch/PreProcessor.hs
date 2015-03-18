{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
module System.DirWatch.PreProcessor (
    PreProcessor
  , PreProcessorM
  , runPreProcessor
  , getTime
  , getLbs
  , yieldSource
  , yieldConduit
  , yieldFilePath
  , (=>=)
) where

import Control.Applicative (Applicative)
import Control.Monad (forM_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Reader (asks, ReaderT, runReaderT)
import Control.Monad.Writer (MonadWriter(tell), WriterT, execWriterT)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime)
import Data.Conduit (Conduit, Source, (=$=), ($$))
import qualified Data.Conduit.Binary as CB

type PreProcessor m = FilePath -> PreProcessorM m ()

type PreProcessorOutput m = [(FilePath, Source m ByteString)]

data PreProcessorEnv m
  = PreProcessorEnv {
      ppeTime   :: UTCTime
    , ppeSource :: Source m ByteString
  } deriving Typeable

newtype PreProcessorM (m :: * -> *) a
  = PreProcessorM {
      unPreProcessorM :: WriterT (PreProcessorOutput m) (
        ReaderT (PreProcessorEnv m) m
        ) a
  } deriving (Functor, Applicative, Monad, Typeable)

runPreProcessor
  :: Monad m
  => Source m ByteString -> UTCTime -> PreProcessorM m ()
  -> m (PreProcessorOutput m)
runPreProcessor source time
  = flip runReaderT env . execWriterT . unPreProcessorM
  where env = PreProcessorEnv {ppeTime=time, ppeSource=source}

getTime :: Monad m => PreProcessorM m UTCTime
getTime = PreProcessorM (asks ppeTime)

getSource :: Monad m => PreProcessorM m (Source m ByteString)
getSource = PreProcessorM (asks ppeSource)

getLbs :: MonadResource m => PreProcessorM m LBS.ByteString
getLbs = PreProcessorM (asks ppeSource) >>= liftProcessor . ($$ CB.sinkLbs)

yieldSource :: Monad m => FilePath -> Source m ByteString -> PreProcessorM m ()
yieldSource filepath source = PreProcessorM $ tell [(filepath, source)]


yieldFilePath :: Monad m => FilePath -> PreProcessorM m ()
yieldFilePath filepath = getSource >>= yieldSource filepath

yieldConduit
  :: Monad m
  => FilePath -> Conduit ByteString m ByteString -> PreProcessorM m ()
yieldConduit filepath conduit
  = getSource >>= yieldSource filepath . (=$= conduit)

infixr 0 =>=
(=>=) :: Monad m => PreProcessor m -> PreProcessor m -> PreProcessor m
ppA =>= ppB = \filepath -> do
  time <- getTime
  src <- getSource
  pairsA <- liftProcessor $ runPreProcessor src time (ppA filepath)
  forM_ pairsA $ \(filepathA, srcA) -> do
    pairsB <- liftProcessor $ runPreProcessor srcA time (ppB filepathA)
    mapM_ (uncurry yieldSource) pairsB
{-# INLINE (=>=) #-}

liftProcessor :: Monad m => m a -> PreProcessorM m a
liftProcessor = PreProcessorM . lift . lift
