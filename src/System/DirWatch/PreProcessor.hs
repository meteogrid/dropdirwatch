{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module System.DirWatch.PreProcessor (
    PreProcessor
  , PreProcessorM
  , runPreProcessor
  , currentTime
  , yieldConduit
  , yieldFilePath
) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader(..), Reader, runReader)
import Control.Monad.Writer (MonadWriter(..), WriterT, execWriterT)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import System.DirWatch.Processor (ProcessorConduit)
import qualified Data.Conduit.List as CL

type PreProcessor = FilePath -> PreProcessorM ()

type PreProcessorW = [(FilePath, ProcessorConduit ByteString ByteString)]

newtype PreProcessorM a
  = PreProcessorM {
      unPreProcessorM :: WriterT PreProcessorW (Reader UTCTime) a
  } deriving (Functor, Applicative, Monad, Typeable)

runPreProcessor :: UTCTime -> PreProcessorM () -> PreProcessorW
runPreProcessor time = flip runReader time . execWriterT . unPreProcessorM

currentTime :: PreProcessorM UTCTime
currentTime = PreProcessorM ask

idConduit :: ProcessorConduit ByteString ByteString
idConduit = CL.map id
{-# INLINE idConduit #-}

yieldFilePath :: FilePath -> PreProcessorM ()
yieldFilePath filepath = PreProcessorM $ tell [(filepath, idConduit)]

yieldConduit ::
  FilePath -> ProcessorConduit ByteString ByteString -> PreProcessorM ()
yieldConduit filepath conduit = PreProcessorM $ tell [(filepath, conduit)]
