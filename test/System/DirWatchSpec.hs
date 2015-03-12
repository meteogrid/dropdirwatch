module System.DirWatchSpec (main, spec) where

import Test.Hspec
import System.DirWatch

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config" $ do
    eConfig <- runIO $ decodeFileEither "test/config.yaml"
    it "can load config" $ do
      case eConfig of
        Right sConfig -> do
          config <- loadConfig sConfig
          return ()
        Left e -> expectationFailure $ "Could not parse YAML: " ++ show e
