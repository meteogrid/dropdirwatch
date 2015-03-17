{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module System.DirWatch.Compiler (
    EvalEnv (..)
  , interpret
) where

import Control.Exception (IOException, Handler(Handler), catches)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText , fromText)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Typeable (Typeable, typeOf)
import Data.Monoid (mempty, mappend)
import Data.Default (Default(..))
import GHC hiding (importPaths)
import GHC.Paths (libdir)
import ErrUtils
import HscTypes
import Outputable (initSDocContext, runSDoc)
import DynFlags
import System.IO.Temp (withSystemTempDirectory)
import Unsafe.Coerce (unsafeCoerce)

data EvalEnv
 = EvalEnv {
     envLibdir      :: FilePath
   , envSearchPath  :: [FilePath]
   , envImports     :: [String]
   , envTargets     :: [String]
   } deriving (Show)


instance Default EvalEnv where
  def
    = EvalEnv {
        envLibdir      = libdir
      , envSearchPath  = []
      , envImports     = []
      , envTargets     = []
      }

interpret
  :: forall a. Typeable a
  => EvalEnv
  -> String
  -> IO (Either [Text] a)
interpret env code = withSystemTempDirectory ".dropdirwatch_objs" $ \dir -> do
  logRef <- newIORef mempty :: (IO (IORef Builder))
  let compileAndLoad = do
        dflags <- getSessionDynFlags
        let dflags' =
#if DYNAMIC_LINKING
                      dynamicTooMkDynamicDynFlags
#else
                      id
#endif
#if COMPILE_PLUGINS
                    . updOptLevel 2
#else
                    . id
#endif
                    . setTmpDir dir
                    $ dflags {
                        mainFunIs     = Nothing
                      --, safeHaskell   = Sf_Safe
                      , outputHi      = Nothing
                      , outputFile    = Nothing
                      , ghcLink       = LinkInMemory
                      , ghcMode       = CompManager
#if COMPILE_PLUGINS
                      , hscTarget     = HscAsm
#else
                      , hscTarget     = HscInterpreted
#endif
                      , objectDir     = Just dir
                      , hiDir         = Just dir
                      , importPaths   = envSearchPath env
                      , log_action    = logHandler logRef
                      , verbosity     = 3
                      }
        void $ setSessionDynFlags dflags'
        defaultCleanupHandler dflags' $ do
          targets <- mapM (\m -> guessTarget m Nothing) (envTargets env)
          setTargets targets
          void $ load LoadAllTargets
          importModules (envImports env ++ envTargets env)
          fmap (Right . unsafeCoerce) $
            compileExpr $ parens code ++ " :: " ++ show (typeOf (undefined :: a))
      handleEx e = do
        msg <- fmap (map LT.toStrict . LT.lines . toLazyText) $ readIORef logRef
        return $ Left (if not (null msg) then msg else [T.pack e])

  (runGhc (Just (envLibdir env)) compileAndLoad) `catches` [
        Handler (\(e :: SourceError) -> handleEx (show e))
      , Handler (\(e :: GhcApiError) -> handleEx (show e))
      , Handler (\(e :: IOException) -> handleEx (show e))
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
        printDoc = fromText . T.pack . show $ runSDoc locMsg cntx

-- |stolen from hint
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n;} in ", foo, ")"]
  where foo = "e_1" ++ filter isDigit s
