module Main (main) where

import System.DirWatch (mainWithCompiler)
import System.DirWatch.Interpreter (interpretConfig)

main :: IO ()
main = mainWithCompiler interpretConfig []
