{-# LANGUAGE NamedFieldPuns #-}
module EvalExpr where

import           Control.Monad        (join)
import           Control.Monad.Trans  (MonadIO, liftIO)
import           Debugger             (showTerm)
import           DynFlags             (ExtensionFlag (..), PkgConfRef (..),
                                       xopt_set)
import           GHC
import           GHC.Paths            (libdir)
import           Outputable           (neverQualify, showSDocForUser)
import           Packages             (initPackages)
import           System.FilePath.Glob (glob)


data EvalInput = EvalInput
  { inputModule :: String
  , inputStmts  :: String
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


evalExpr :: Read a => [EvalInput] -> IO [a]
evalExpr exprs = do
  pkgDbs <- join <$> mapM glob
    [ "dist/package.conf.inplace"
    , ".cabal-sandbox/*-packages.conf.d"
    , "../.cabal-sandbox/*-packages.conf.d"
    ]
  GHC.runGhc (Just libdir) $ do
    addPkgDbs pkgDbs
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags $ foldl xopt_set dflags
      { hscTarget = HscInterpreted
      , ghcLink   = LinkInMemory
      , ghcMode   = CompManager
      } [Opt_ImplicitPrelude]

    _ <- load LoadAllTargets
    mapM evalOne exprs


evalOne :: (Read a, GhcMonad m) => EvalInput -> m a
evalOne EvalInput { inputModule, inputStmts } = do
  setContext $ map (IIDecl . simpleImportDecl . mkModuleName)
    [ "Prelude"
    , "Text.Pandoc"
    , "Text.Printf"
    , inputModule
    ]
  ns <- runDecls inputStmts
  case ns of
    n:_ -> getResult n
    _   -> fail "no declarations found"


getResult :: (Read a, GhcMonad m) => Name -> m a
getResult n = do
  df <- getSessionDynFlags
  Just (AnId aid) <- lookupName n
  fmap (read . pp df) $ showTerm =<< obtainTermFromId maxBound True aid

  where
    pp df = showSDocForUser df neverQualify
