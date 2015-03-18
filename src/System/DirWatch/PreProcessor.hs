{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Monad.Reader (asks, ReaderT, runReaderT)
import Control.Monad.Writer (MonadWriter(tell), WriterT, execWriterT)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime)
import System.DirWatch.Processor (ProcessorConduit, ProcessorSource, ProcessorM)
import Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.Binary as CB

type PreProcessor = FilePath -> PreProcessorM ()

type PreProcessorOutput = [(FilePath, ProcessorSource)]

data PreProcessorEnv
  = PreProcessorEnv {
      ppeTime   :: UTCTime
    , ppeSource :: ProcessorSource
  } deriving Typeable

newtype PreProcessorM a
  = PreProcessorM {
      unPreProcessorM :: WriterT PreProcessorOutput (
        ReaderT PreProcessorEnv ProcessorM
        ) a
  } deriving (Functor, Applicative, Monad, Typeable)

runPreProcessor
  :: ProcessorSource -> UTCTime -> PreProcessorM ()
  -> ProcessorM PreProcessorOutput
runPreProcessor source time
  = flip runReaderT env . execWriterT . unPreProcessorM
  where env = PreProcessorEnv {ppeTime=time, ppeSource=source}

getTime :: PreProcessorM UTCTime
getTime = PreProcessorM (asks ppeTime)

getSource :: PreProcessorM ProcessorSource
getSource = PreProcessorM (asks ppeSource)

getLbs :: PreProcessorM LBS.ByteString
getLbs = PreProcessorM (asks ppeSource) >>= liftProcessor . ($$ CB.sinkLbs)

yieldSource :: FilePath -> ProcessorSource -> PreProcessorM ()
yieldSource filepath source = PreProcessorM $ tell [(filepath, source)]


yieldFilePath :: FilePath -> PreProcessorM ()
yieldFilePath filepath = getSource >>= yieldSource filepath

yieldConduit ::
  FilePath -> ProcessorConduit ByteString ByteString -> PreProcessorM ()
yieldConduit filepath conduit
  = getSource >>= yieldSource filepath . (=$= conduit)

infixr 0 =>=
(=>=) :: PreProcessor -> PreProcessor -> PreProcessor
ppA =>= ppB = \filepath -> do
  time <- getTime
  src <- getSource
  pairsA <- liftProcessor $ runPreProcessor src time (ppA filepath)
  forM_ pairsA $ \(filepathA, srcA) -> do
    pairsB <- liftProcessor $ runPreProcessor srcA time (ppB filepathA)
    mapM_ (uncurry yieldSource) pairsB
{-# INLINE (=>=) #-}

liftProcessor :: ProcessorM a -> PreProcessorM a
liftProcessor = PreProcessorM . lift . lift
