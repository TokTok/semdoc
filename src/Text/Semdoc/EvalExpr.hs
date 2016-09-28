{-# LANGUAGE NamedFieldPuns #-}
module Text.Semdoc.EvalExpr where

import           Control.Applicative  ((<$>))
import           Control.Monad        (join)
import           Debugger             (showTerm)
import           DynFlags             (ExtensionFlag (..), PkgConfRef (..),
                                       xopt_set)
import           GHC
import           GHC.Paths            (libdir)
import           MonadUtils
import           Outputable           (neverQualify, showSDocForUser)
import           Packages             (initPackages)
import           System.FilePath.Glob (glob)


data EvalInput a = EvalInput
  { inputModule :: String
  , inputStmts  :: String
  }
  deriving (Show)


addPkgDbs :: GhcMonad m => [FilePath] -> m ()
addPkgDbs fps = do
  dfs <- getSessionDynFlags
  let pkgs = map PkgConfFile fps
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
  _ <- setSessionDynFlags dfs'
  _ <- liftIO $ initPackages dfs'
  return ()


evalExpr :: Read a => [EvalInput a] -> IO [a]
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


evalOne :: (Read a, GhcMonad m) => EvalInput a -> m a
evalOne EvalInput { inputModule, inputStmts } = do
  setContext $ map mkImportDecl
    [ ("Data.Char"       , Just "Char")
    , ("Data.List"       , Just "List")
    , ("Prelude"         , Nothing)
    , ("Text.Pandoc"     , Nothing)
    , ("Text.Printf"     , Nothing)
    , ("Text.Semdoc.Util", Nothing)
    , (inputModule       , Nothing)
    ]
  ns <- reverse <$> runDecls inputStmts
  case ns of
    n:_ -> getResult n
    _   -> fail "no declarations found"

  where
    mkImportDecl (m, n) =
      IIDecl (simpleImportDecl $ mkModuleName m)
        { ideclAs = fmap mkModuleName n
        , ideclQualified = n /= Nothing
        }


getResult :: (Read a, GhcMonad m) => Name -> m a
getResult n = do
  df <- getSessionDynFlags
  Just (AnId aid) <- lookupName n
  result <- fmap (pp df) $ showTerm =<< obtainTermFromId maxBound True aid
  --liftIO $ putStrLn result
  return $ read result

  where
    pp df = showSDocForUser df neverQualify
