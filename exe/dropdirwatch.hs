module Main (main) where

import System.DirWatch (mainWithCompiler)
import System.DirWatch.Interpreter (compileConfig)

defaultConfigFile :: String
defaultConfigFile = "/etc/dropdirwatch.yaml"

main :: IO ()
main = mainWithCompiler compileConfig defaultConfigFile
