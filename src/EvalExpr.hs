{-# LANGUAGE NamedFieldPuns #-}
module EvalExpr where

import           Control.Monad       (join)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Debugger
import           DynFlags
import           GHC
import           GHC.Paths           (libdir)
import           Name                (getOccString)
import           Outputable
import           Packages
import           TyCon               (tyConName)
import           TypeRep             (Type (..))


data EvalInput = EvalInput
  { inputModule :: String
  , inputName   :: String
  , inputExpr   :: String
  }
  deriving (Show)


data EvalResult = EvalResult
  { resultName  :: String
  , resultType  :: String
  , resultValue :: String
  }
  deriving (Show)


addPkgDbs :: (MonadIO m, GhcMonad m) => [FilePath] -> m ()
addPkgDbs fps = do
  dfs <- getSessionDynFlags
  let pkgs = map PkgConfFile fps
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
  _ <- setSessionDynFlags dfs'
  _ <- liftIO $ initPackages dfs'
  return ()


evalExpr :: [EvalInput] -> IO [EvalResult]
evalExpr exprs =
  GHC.runGhc (Just libdir) $ do
    addPkgDbs
      [ "dist/package.conf.inplace"
      , "../.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d"
      ]
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags $ foldl xopt_set dflags
      { hscTarget = HscInterpreted
      , ghcLink   = LinkInMemory
      , ghcMode   = CompManager
      } [Opt_ImplicitPrelude]

    _ <- load LoadAllTargets
    join <$> mapM evalOne exprs


evalOne :: GhcMonad m => EvalInput -> m [EvalResult]
evalOne EvalInput { inputModule, inputName, inputExpr } = do
  setContext $
    map (IIDecl . simpleImportDecl . mkModuleName) ["Prelude", inputModule]
  let stmt = "let " ++ inputName ++ " = (" ++ inputExpr ++ ")"
  rr <- runStmt stmt RunToCompletion
  case rr of
    RunOk ns       -> showNS ns
    RunException e -> fail $ show e
    _              -> fail "unknown error"


showNS :: GhcMonad m => [Name] -> m [EvalResult]
showNS =
  mapM $ \n -> do
    df <- getSessionDynFlags
    Just (AnId aid) <- lookupName n
    evalDoc <- showTerm =<< obtainTermFromId maxBound True aid
    let ty = idType aid
    let value = pp df evalDoc
    return EvalResult
      { resultName = pp df (ppr n)
      , resultType = pp df (ppr ty)
      , resultValue =
          if isStringTy ty
            then read value
            else value
      }

  where
    pp df = showSDocForUser df neverQualify

    isStringTy (TyConApp tycon _) = (getOccString . tyConName) tycon == "String"
    isStringTy _                  = False
