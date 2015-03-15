module System.DirWatch.UtilSpec (main, spec) where

import Test.Hspec
import System.DirWatch.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "patternDir" $ do
    it "behaves as expecyed with hand-crafted inputs" $ do
      patternDir "/usr/local/*" `shouldBe` "/usr/local/"
      patternDir "/*/local/config" `shouldBe` "/"
      patternDir "/usr/local/configs" `shouldBe` "/usr/local/configs"
