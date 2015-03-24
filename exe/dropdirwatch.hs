module Main (main) where

import System.DirWatch (mainDirWatcher)
import System.DirWatch.Interpreter (interpretConfig)

main :: IO ()
main = mainDirWatcher interpretConfig []
