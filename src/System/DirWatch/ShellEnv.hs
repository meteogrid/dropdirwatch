{-# LANGUAGE OverloadedStrings #-}
module System.DirWatch.ShellEnv (
    ShellEnv
  , envSet
  , envAppend
  , sysEnvironment
  , shellEnvToEnv
) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (
    FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , object
  )
import qualified Data.HashMap.Strict as HM
import Data.Text (pack, unpack, uncons)
import Data.List (find)
import Data.Monoid (Monoid(..))
import System.Environment (getEnvironment)

newtype ShellEnv = ShellEnv [EnvItem] deriving (Show)

instance Eq ShellEnv where
  ShellEnv as == ShellEnv bs
    = length as == length bs && all (`elem` bs) as

data EnvItem
  = EnvSet    {envKey :: String, envVal :: String}
  | EnvAppend {envKey :: String, envVal :: String}
  deriving (Show, Eq)

instance Monoid ShellEnv where
  mempty  = ShellEnv []
  ShellEnv as `mappend` ShellEnv bs = ShellEnv $ as' ++ bs'
    where
      bs' = map mkB bs
      as' = filter ((`notElem` bKeys) . envKey) as
      bKeys = map envKey bs
      mkB (EnvAppend k v)
        = case find ((==k) . envKey) as of
            Just e  -> EnvAppend (envKey e) (envVal e ++ ":" ++ v)
            Nothing -> EnvAppend k v
      mkB e = e

instance ToJSON ShellEnv where
  toJSON (ShellEnv items) = object $ map toPair items
    where toPair (EnvSet k v)    = (pack k, toJSON v)
          toPair (EnvAppend k v) = (pack ('+':k), toJSON v)

instance FromJSON ShellEnv where
  parseJSON (Object o)
    = ShellEnv <$> mapM fromPair (HM.toList o)
    where
      fromPair (k, v) = case uncons k of
        Just ('+',k') -> EnvAppend <$> pure (unpack k') <*> parseJSON v
        Just _        -> EnvSet    <$> pure (unpack k)  <*> parseJSON v
        Nothing       -> fail "Unexpected empty jey for ShellEnv"
  parseJSON _ = fail "Expected for env"

sysEnvironment :: (Functor m, MonadIO m) => m ShellEnv
sysEnvironment = fmap (ShellEnv . map (uncurry EnvSet)) $ liftIO getEnvironment

shellEnvToEnv :: ShellEnv -> [(String, String)]
shellEnvToEnv (ShellEnv ls) = zip (map envKey ls) (map envVal ls)

envSet, envAppend :: String -> String -> ShellEnv -> ShellEnv
envSet k v    = (`mappend` ShellEnv [EnvSet k v])
envAppend k v = (`mappend` ShellEnv [EnvAppend k v])
