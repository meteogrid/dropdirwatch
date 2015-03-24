{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
module System.DirWatch.PreProcessor (
    PreProcessor
  , PreProcessorT
  , runPreProcessor
  , getTime
  , getLbs
  , yieldSource
  , yieldConduit
  , yieldFilePath
  , yieldFileName
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
import System.DirWatch.Types (HasCurrentTime(..))
import System.FilePath.Posix (takeFileName)

type PreProcessor m = FilePath -> PreProcessorT m ()

type PreProcessorOutput m = [(FilePath, Source m ByteString)]

data PreProcessorEnv m
  = PreProcessorEnv {
      ppeTime   :: UTCTime
    , ppeSource :: Source m ByteString
  } deriving Typeable

newtype PreProcessorT (m :: * -> *) a
  = PreProcessorT {
      unPreProcessorT :: WriterT (PreProcessorOutput m) (
        ReaderT (PreProcessorEnv m) m
        ) a
  } deriving (Functor, Applicative, Monad, Typeable)

runPreProcessor
  :: Monad m
  => Source m ByteString -> UTCTime -> PreProcessorT m ()
  -> m (PreProcessorOutput m)
runPreProcessor source time
  = flip runReaderT env . execWriterT . unPreProcessorT
  where env = PreProcessorEnv {ppeTime=time, ppeSource=source}

instance Monad m => HasCurrentTime (PreProcessorT m) where
  getTime = PreProcessorT (asks ppeTime)

getSource :: Monad m => PreProcessorT m (Source m ByteString)
getSource = PreProcessorT (asks ppeSource)

getLbs :: MonadResource m => PreProcessorT m LBS.ByteString
getLbs = PreProcessorT (asks ppeSource) >>= liftProcessor . ($$ CB.sinkLbs)

yieldSource :: Monad m => FilePath -> Source m ByteString -> PreProcessorT m ()
yieldSource filepath source = PreProcessorT $ tell [(filepath, source)]


yieldFilePath :: Monad m => FilePath -> PreProcessorT m ()
yieldFilePath filepath = getSource >>= yieldSource filepath

yieldFileName  :: Monad m => PreProcessor m
yieldFileName = yieldFilePath . takeFileName

yieldConduit
  :: Monad m
  => FilePath -> Conduit ByteString m ByteString -> PreProcessorT m ()
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

liftProcessor :: Monad m => m a -> PreProcessorT m a
liftProcessor = PreProcessorT . lift . lift
