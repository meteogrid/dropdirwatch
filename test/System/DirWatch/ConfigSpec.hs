module System.DirWatch.ConfigSpec (main, spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.Yaml (decodeFileEither)
import System.DirWatch.Config (Config(..))
import System.DirWatch.Interpreter (compileConfig)
import System.DirWatch.ShellEnv (envSet, envAppend)
import System.FilePath.Glob (namesMatching)
import Data.Monoid (mempty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config" $ do
    describe "valid config" $ do
      eConfig <- runIO $ decodeFileEither "test/config.yaml"

      it "can load config" $ withValidConfig eConfig $ \config -> do
        rConfig <- compileConfig config
        case rConfig of
          Right _ -> return ()
          Left e  -> expectationFailure $ show e

      it "env is parsed correctly" $ withValidConfig eConfig $ \config -> do
        let expected = envSet    "FOO"  "bar"
                     $ envAppend "PATH" "/opt/foo"
                     $ mempty
            env      = cfgShellEnv config
        env `shouldBe` expected

    describe "invalid configs" $ do
      badFiles <- runIO $ namesMatching "test/bad*.yaml"
      forM_ badFiles $ \file ->
        it ("fails with invalid file " ++ file) $ do
          eConfig <- decodeFileEither file
          case eConfig of
            Right cConfig -> do
              rConfig <- compileConfig cConfig
              case rConfig of
                Right _ -> expectationFailure "Should have failed"
                Left  _ -> return ()
            Left  _ -> return ()
              

withValidConfig :: Show a => Either a t -> (t -> Expectation) -> Expectation
withValidConfig (Right c) act = act c
withValidConfig (Left e) _
  = expectationFailure $ "Could not parse YAML: " ++ show e
