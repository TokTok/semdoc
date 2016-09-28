module Text.Semdoc.InlineHaskell where

import qualified Control.Monad.State.Strict as CMS
import           Data.Default.Class         (Default (..))
import           Text.Groom                 (groom)
import           Text.Pandoc                (Block (..), Format (..),
                                             Inline (..))
import           Text.Pandoc.Walk           (walkM)
import           Text.Regex.TDFA            ((=~))

import           Text.Semdoc.EvalExpr       (EvalInput (..))


data Input = Input
  { inputsBlock   :: [EvalInput Block]
  , inputsInline  :: [EvalInput Inline]
  , currentModule :: String
  , allModules    :: [String]
  }

instance Default Input where
  def = Input
    { inputsBlock   = []
    , inputsInline  = []
    , currentModule = "Prelude"
    , allModules    = []
    }


extractExprsInline :: Inline -> CMS.State Input Inline
extractExprsInline i =
  case i of
    RawInline (Format "latex") ('\\':'s':'h':'o':'w':'{':expr) -> doExtract expr
    Code ("", [], []) ('\\':'s':'h':'o':'w':'{':expr)          -> doExtract expr
    _                                                          -> return i

  where
    doExtract :: String -> CMS.State Input Inline
    doExtract expr = do
      let stmt = "it = " ++ init expr
      input <- CMS.get
      CMS.put input
        { inputsInline =
            EvalInput (currentModule input) stmt : inputsInline input
        }
      return i


extractExprs :: Block -> CMS.State Input Block
extractExprs i =
  case i of
    CodeBlock ("", [], [("semdoc", "")]) code ->
      doExtract code
    CodeBlock ("", ["sourceCode", "literate", "haskell"], []) code ->
      case code =~ "module ([A-Z][A-Za-z0-9_.]+)" of
        [[_, modName]] -> do
          input <- CMS.get
          CMS.put input
            { currentModule = modName
            , allModules    = modName : allModules input
            }
          return i
        _ ->
          return i
    CodeBlock   {} -> return i
    BulletList  {} -> return i
    OrderedList {} -> return i
    Table       {} -> return i
    Plain       {} -> walkM extractExprsInline i
    Para        {} -> walkM extractExprsInline i
    Header      {} -> walkM extractExprsInline i
    _              -> error $ groom i

  where
    doExtract :: String -> CMS.State Input Block
    doExtract stmt = do
      input <- CMS.get
      CMS.put input
        { inputsBlock =
            EvalInput (currentModule input) stmt : inputsBlock input
        }
      return i


replaceExprsInline :: Inline -> CMS.State ([Block], [Inline]) Inline
replaceExprsInline i =
  case i of
    RawInline (Format "latex") ('\\':'s':'h':'o':'w':'{':_) -> doReplace
    Code ("", [], []) ('\\':'s':'h':'o':'w':'{':_)          -> doReplace
    _                                                       -> return i

  where
    doReplace :: CMS.State ([Block], [Inline]) Inline
    doReplace = do
      (resultsBlock, resultsInline) <- CMS.get
      case resultsInline of
        [] -> fail "insufficient results"
        r:rs -> do
          CMS.put (resultsBlock, rs)
          return r


replaceExprs :: Block -> CMS.State ([Block], [Inline]) Block
replaceExprs i =
  case i of
    CodeBlock ("", [], [("semdoc", "")]) _ -> doReplace
    CodeBlock   {}                         -> return i
    BulletList  {}                         -> return i
    OrderedList {}                         -> return i
    Table       {}                         -> return i
    Plain       {}                         -> walkM replaceExprsInline i
    Para        {}                         -> walkM replaceExprsInline i
    Header      {}                         -> walkM replaceExprsInline i
    _                                      -> error $ groom i

  where
    doReplace :: CMS.State ([Block], [Inline]) Block
    doReplace = do
      (resultsBlock, resultsInline) <- CMS.get
      case resultsBlock of
        [] -> fail "insufficient results"
        r:rs -> do
          CMS.put (rs, resultsInline)
          return r
