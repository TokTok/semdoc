module Text.Semdoc.InlineHaskell where

import qualified Control.Monad.State.Strict as CMS
import           Text.Groom                 (groom)
import           Text.Pandoc                (Block (..), Format (..),
                                             Inline (..))
import           Text.Pandoc.Walk           (walkM)
import           Text.Regex.TDFA            ((=~))

import           Text.Semdoc.EvalExpr       (EvalInput (..))


extractExprsInline :: Inline -> CMS.State ([EvalInput Block], [EvalInput Inline], String) Inline
extractExprsInline i =
  case i of
    RawInline (Format "latex") ('\\':'s':'h':'o':'w':'{':expr) -> doExtract expr
    Code ("", [], []) ('\\':'s':'h':'o':'w':'{':expr)          -> doExtract expr
    _                                                          -> return i

  where
    doExtract :: String -> CMS.State ([EvalInput Block], [EvalInput Inline], String) Inline
    doExtract expr = do
      (inputsBlock, inputsInline, modName) <- CMS.get
      CMS.put
        ( inputsBlock
        , EvalInput modName ("it = " ++ init expr) : inputsInline
        , modName
        )
      return i


extractExprs :: Block -> CMS.State ([EvalInput Block], [EvalInput Inline], String) Block
extractExprs i =
  case i of
    CodeBlock ("", [], [("semdoc", "")]) code ->
      doExtract code
    CodeBlock ("", ["sourceCode", "literate", "haskell"], []) code ->
      case code =~ "module ([A-Z][A-Za-z0-9_.]+)" of
        [[_, modName]] -> do
          (inputBlock, inputsInline, _) <- CMS.get
          CMS.put (inputBlock, inputsInline, modName)
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
    doExtract :: String -> CMS.State ([EvalInput Block], [EvalInput Inline], String) Block
    doExtract stmt = do
      (inputsBlock, inputsInline, modName) <- CMS.get
      CMS.put
        ( EvalInput modName stmt : inputsBlock
        , inputsInline
        , modName
        )
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
