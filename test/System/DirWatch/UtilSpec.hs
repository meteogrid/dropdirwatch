{-# LANGUAGE OverloadedStrings #-}
module System.DirWatch.UtilSpec (main, spec) where

import Test.Hspec
import System.DirWatch.Util
import Data.Time.Calendar (fromGregorian)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "globMatch" $ do
    it "matches a visible file" $
      "/foo.bin" `globMatch` "/*.bin" `shouldBe` True
    it "does not match an invisible file" $
      "/.foo.bin" `globMatch` "/*.bin"  `shouldBe` False
    it "matches an invisible file if the pattern explicitly allows it" $
      "/.foo.bin" `globMatch` "/.*.bin"  `shouldBe` True

  describe "takePatternDirectory" $ do
    it "takes longest directory name" $
      takePatternDirectory "/usr/local/*/foo" `shouldBe` "/usr/local"
    it "will be root if wildcard on first token" $
      takePatternDirectory "/*/local/config" `shouldBe` "/"
    it "strips trailing slash" $
      takePatternDirectory "/usr/local/config/" `shouldBe` "/usr/local/config"
    it "treats paths without trailing slash as files" $
      takePatternDirectory "/usr/local/config" `shouldBe` "/usr/local"

  describe "archiveDestination" $ do
    let day = fromGregorian 2015 3 15

    it "subdirectory is relative if archive dir and filename share prefix" $
      archiveDestination "/srv/ftp/archive" day
          "/srv/ftp/dropbox/luther/foo.txt"
        `shouldBe`
          "/srv/ftp/archive/dropbox/luther/2015/3/15/foo.txt"

    it "subdirectory is absolute if archive dir and filename dont share prefix"$
      archiveDestination "/archive" day
          "/srv/ftp/dropbox/luther/foo.txt"
        `shouldBe`
          "/archive/srv/ftp/dropbox/luther/2015/3/15/foo.txt"

    it "dir can have a trailing slash" $
      archiveDestination "/srv/ftp/archive/" day
          "/srv/ftp/dropbox/luther/foo.txt"
        `shouldBe`
          "/srv/ftp/archive/dropbox/luther/2015/3/15/foo.txt"

