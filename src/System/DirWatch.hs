module System.DirWatch (
    compileConfig
  , decodeFileEither
) where

import System.DirWatch.Interpreter
import System.DirWatch.Config

import Data.Yaml (decodeFileEither)
