{-# LANGUAGE OverloadedStrings #-}
module System.DirWatch.ProcessorSpec (main, spec) where

import Test.Hspec
import Control.Exception (ErrorCall(..))
import Data.Monoid (mempty)
import System.DirWatch.Processor
import System.DirWatch.ShellEnv
import Data.ByteString.Char8 as BS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "exceptions" $ do
    it "handles normal exceptions" $ do
      result <- runProcessorM def undefined $ error "foo"
      case result of
        Left (ProcessorException e) -> show e `shouldBe` "foo"
        _ -> expectationFailure "Expected to catch error"

    it "can catch normal exceptions" $ do
      let handleEx (ErrorCall "foo") = return True
          handleEx _                 = return False
      result <- runProcessorM def undefined $ (error "foo") `catchE` handleEx
      case result of
        Right True -> return ()
        e -> expectationFailure $ "Unexpected result: " ++ show e

  describe "executeShellCmd" $ do
    it "can execute simple shell command" $ do
      eResult <- runProcessorM def undefined $ executeShellCmd (shellCmd "ls")
      case eResult of 
        Left e -> expectationFailure $ "Unexpected shell error: " ++ show e
        Right (stdout, stderr) -> do
          stderr `shouldBe` ""
          BS.null (snd (BS.breakSubstring "LICENSE" stdout)) `shouldBe` False

    it "can use environment variables in command" $ do
      let cmd = (shellCmd "echo -n $FOO") {shEnv=env}
          env = envSet "FOO" "bar" mempty
      eResult <- runProcessorM def undefined $ executeShellCmd cmd
      case eResult of 
        Left e -> expectationFailure $ "Unexpected shell error: " ++ show e
        Right (stdout, stderr) -> do
          stderr `shouldBe` ""
          stdout `shouldBe` "bar"
