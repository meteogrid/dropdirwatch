module System.DirWatchSpec (main, spec) where

import Test.Hspec
import System.DirWatch

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config" $ do
    Right sConfig <- runIO $ decodeFileEither "test/config.yaml"
    it "can load config" $ do
      config <- loadConfig sConfig
      return ()
