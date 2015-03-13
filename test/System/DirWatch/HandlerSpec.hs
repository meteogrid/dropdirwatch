module System.DirWatch.HandlerSpec (main, spec) where

import Test.Hspec
import System.DirWatch.Handler
import Data.Default (def)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "exceptions" $ do
  it "handles normal exceptions" $ do
    result <- runHandlerM def $ error "foo"
    case result of
      Left (HandlerException e) -> show e `shouldBe` "foo"
      _ -> expectationFailure "Expected to catch error"

  it "can catch normal exceptions" $ do
    let handleEx (HandlerException e) = return $ show e == "foo"
        handleEx _                    = return False
    result <- runHandlerM def $ (error "foo") `catchE` handleEx
    case result of
      Right True -> return ()
      e -> expectationFailure $ "Unexpected result: " ++ show e

