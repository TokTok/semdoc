{-# LANGUAGE NamedFieldPuns #-}
module EvalExpr where

import           Control.Monad       (join)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Debugger            (showTerm)
import           DynFlags            (ExtensionFlag (..), PkgConfRef (..),
                                      xopt_set)
import           GHC
import           GHC.Paths           (libdir)
import           Outputable          (neverQualify, ppr, showSDocForUser)
import           Packages            (initPackages)


data EvalInput = EvalInput
  { inputModule :: String
  , inputName   :: String
  , inputExpr   :: String
  }
  deriving (Show)


data EvalResult a = EvalResult
  { resultName  :: String
  , resultType  :: String
  , resultValue :: a
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


evalExpr :: Read a => [EvalInput] -> IO [EvalResult a]
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


evalOne :: (Read a, GhcMonad m) => EvalInput -> m [EvalResult a]
evalOne EvalInput { inputModule, inputName, inputExpr } = do
  setContext $ map (IIDecl . simpleImportDecl . mkModuleName)
    [ "Prelude"
    , "Text.Pandoc"
    , "Text.Printf"
    , inputModule
    ]
  let stmt = "let " ++ inputName ++ " = (" ++ inputExpr ++ ")"
  rr <- runStmt stmt RunToCompletion
  case rr of
    RunOk ns       -> showNS ns
    RunException e -> fail $ show e
    _              -> fail "unknown error"


showNS :: (Read a, GhcMonad m) => [Name] -> m [EvalResult a]
showNS =
  mapM $ \n -> do
    df <- getSessionDynFlags
    Just (AnId aid) <- lookupName n
    evalDoc <- showTerm =<< obtainTermFromId maxBound True aid
    let ty = idType aid
    return EvalResult
      { resultName = pp df (ppr n)
      , resultType = pp df (ppr ty)
      , resultValue = read $ pp df evalDoc
      }

  where
    pp df = showSDocForUser df neverQualify
