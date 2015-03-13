{-# LANGUAGE DeriveDataTypeable #-}
module System.DirWatch.PreProcessor (
    PreProcessor (..)
) where

import qualified Data.ByteString.Lazy as LBS
import Data.Typeable (Typeable)

newtype PreProcessor
  = PreProcessor {
      preProcess :: FilePath -> LBS.ByteString -> [(FilePath,LBS.ByteString)]
  } deriving Typeable
