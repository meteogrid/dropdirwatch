{-# LANGUAGE OverloadedStrings #-}
module System.DirWatch.UtilSpec (main, spec) where

import Test.Hspec
import System.DirWatch.Util
import Data.Time.Calendar (fromGregorian)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "takePatternDirectory" $ do
    it "takes longest directory name" $ do
      takePatternDirectory "/usr/local/*/foo" `shouldBe` "/usr/local/"
    it "will be root if wildcard on first token" $ do
      takePatternDirectory "/*/local/config" `shouldBe` "/"
    it "is id if directory without wildcards" $ do
      takePatternDirectory "/usr/local/config/" `shouldBe` "/usr/local/config/"
    it "is id if a file without wildcards" $ do
      takePatternDirectory "/usr/local/config" `shouldBe` "/usr/local/config"

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

