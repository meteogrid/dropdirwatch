module Main (main) where

import System.DirWatch (mainWithCompiler)
import System.DirWatch.Interpreter (compileConfig)

main :: IO ()
main = mainWithCompiler compileConfig []
