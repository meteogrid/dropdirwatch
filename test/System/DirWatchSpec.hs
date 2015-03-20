module System.DirWatchSpec (main, spec) where

import Test.Hspec
import Data.Yaml (decodeFileEither)
import System.DirWatch.Config (Config(..))
import System.DirWatch.Interpreter (compileConfig)
import System.DirWatch.ShellEnv (envSet, envAppend)
import Data.Monoid (mempty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config" $ do
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

withValidConfig :: Show a => Either a t -> (t -> Expectation) -> Expectation
withValidConfig (Right c) act = act c
withValidConfig (Left e) _
  = expectationFailure $ "Could not parse YAML: " ++ show e
