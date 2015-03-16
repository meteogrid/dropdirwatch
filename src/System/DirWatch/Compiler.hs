{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module System.DirWatch.Compiler (
    EvalEnv (..)
  , defaultEnv
  , interpret
) where

import Data.ByteString.Builder (Builder, toLazyByteString , byteString)
import Control.Exception (Handler(Handler), catches)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils
import HscTypes
import Outputable
import DynFlags
import Unsafe.Coerce (unsafeCoerce)


data EvalEnv
 = EvalEnv {
     envLibdir      :: FilePath
   , envSearchPath  :: [FilePath]
   , envImports     :: [String]
   , envTargets     :: [String]
   } deriving (Show)


defaultEnv :: EvalEnv
defaultEnv = EvalEnv {
    envLibdir      = libdir
  , envSearchPath  = []
  , envImports     = []
  , envTargets     = []
  }

interpret :: forall a.
    Typeable a =>
    EvalEnv ->
    String ->
    IO (Either ByteString a)

interpret env code = do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' = dynamicTooMkDynamicDynFlags . updOptLevel 2 $ dflags {
                mainFunIs     = Nothing
              , safeHaskell   = Sf_Safe
              , ghcLink       = LinkInMemory
              , ghcMode       = CompManager
              , hscTarget     = HscAsm
              , objectDir     = Just "/tmp/.ghcobjs"
              , hiDir         = Just "/tmp/.ghcobjs"
              , importPaths   = envSearchPath env
              , log_action    = logHandler logRef
              , verbosity     = 0
              }
        void $ setSessionDynFlags dflags'
        defaultCleanupHandler dflags' $ do
          targets <- mapM (\m -> guessTarget m Nothing) (envTargets env)
          setTargets targets
          void $ load LoadAllTargets
          importModules (envImports env ++ envTargets env)
          fmap (Right . unsafeCoerce) $
            compileExpr (code ++ " :: " ++ show (typeOf (undefined :: a)))

      handleEx e = do
        msg <- fmap toLazyByteString $ readIORef logRef
        let bsMsg = LBS.toStrict msg
        return $ Left (if LBS.length msg > 0 then bsMsg else BS.pack e)

  (runGhc (Just (envLibdir env)) compileAndLoad) `catches` [
        Handler (\(e :: SourceError) -> handleEx (show e))
      , Handler (\(e :: GhcApiError) -> handleEx (show e))
    ]

importModules:: [String] -> Ghc ()
importModules mods =
    GHC.setContext . (map (GHC.IIDecl . import_)) $ mods
    where 
        import_ name =
            (GHC.simpleImportDecl . GHC.mkModuleName $ name)
            {GHC.ideclQualified=False}


-- from http://parenz.wordpress.com/2013/07/23/on-custom-error-handlers-for-ghc-api/
logHandler :: IORef Builder -> LogAction
logHandler ref dflags severity srcSpan style msg =
  case severity of
     SevError ->  modifyIORef' ref (mappend printDoc)
     SevFatal ->  modifyIORef' ref (mappend printDoc)
     _        ->  return () -- ignore the rest
  where cntx = initSDocContext dflags style
        locMsg = mkLocMessage severity srcSpan msg
        printDoc = byteString . BS.pack . show $ runSDoc locMsg cntx
