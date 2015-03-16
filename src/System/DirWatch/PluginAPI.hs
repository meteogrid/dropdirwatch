{-# LANGUAGE Trustworthy #-}
module System.DirWatch.PluginAPI (
    modifyBaseName
  , module API
) where

import Control.Applicative as API (pure, (<$>), (<*>), (<|>))
import Data.Monoid as API (mempty, mappend, (<>))
import Data.Aeson as API (
    FromJSON (..)
  , Value (..)
  , (.:)
  , (.:?)
  , (.!=)
  )
import System.FilePath.Posix as API
import System.DirWatch.Processor as API hiding (ProcessorConfig)
import System.DirWatch.ShellEnv as API (envSet, envAppend)

modifyBaseName :: FilePath -> (FilePath -> FilePath) -> FilePath
modifyBaseName fpath func = replaceBaseName fpath (func (takeBaseName fpath))
