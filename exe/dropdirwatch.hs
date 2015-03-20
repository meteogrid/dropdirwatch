module Main (main) where

import System.DirWatch
import System.Environment (getArgs)

defaultConfigFile :: String
defaultConfigFile = "/etc/dropdirwatch.yaml"

main :: IO ()
main = do
  args <- getArgs
  let (configFile, reload) =
        case args of
          ["-reload"]        -> (defaultConfigFile, True)
          [fname]            -> (fname, False)
          [fname, "-reload"] -> (fname, True)
          []                 -> (defaultConfigFile, False)
          _                  -> error $ "Invalid args: " ++ show args
  runWithConfigFile configFile reload
