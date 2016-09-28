{-# LANGUAGE LambdaCase #-}
module Text.Semdoc.Processor where

import qualified Control.Monad.State.Strict as CMS
import qualified Data.Set                   as Set
import           Text.Groom                 (groom)
import           Text.Pandoc                (Block (..))
import qualified Text.Pandoc                as Pandoc
import           Text.Pandoc.Error          (PandocError)
import           Text.Pandoc.Walk           (walk, walkM)

import           Text.Semdoc.EvalExpr       (evalExpr)
import           Text.Semdoc.InlineHaskell  (extractExprs, replaceExprs)


readerOpts :: Pandoc.ReaderOptions
readerOpts = Pandoc.def
  { Pandoc.readerExtensions = Set.fromList [Pandoc.Ext_literate_haskell]
  , Pandoc.readerParseRaw = True
  }


semdocExtensions :: Set.Set Pandoc.Extension
semdocExtensions =
  Set.insert Pandoc.Ext_tex_math_dollars $
    foldl (flip Set.delete) Pandoc.githubMarkdownExtensions
      [ Pandoc.Ext_hard_line_breaks
      , Pandoc.Ext_native_spans
      , Pandoc.Ext_raw_html
      , Pandoc.Ext_tex_math_single_backslash
      ]


eraseCodeBlocks :: Block -> Block
eraseCodeBlocks (CodeBlock ("", ["sourceCode", "literate", "haskell"], []) _) =
  Null
eraseCodeBlocks x = x


processDoc :: Pandoc.Pandoc -> IO String
processDoc doc@(Pandoc.Pandoc _ parsed) = do
  putStrLn $ "[=] processing " ++ show (length parsed) ++ " toplevel blocks"
  --putStrLn $ groom parsed
  let (exprsBlock, exprsInline, _) = snd . CMS.runState (walkM extractExprs doc) $ ([], [], "Prelude")
  putStrLn $ "[=] evaluating " ++ show (length exprsBlock) ++ " block expressions"
  resultsBlock <- evalExpr exprsBlock
  putStrLn $ "[=] evaluating " ++ show (length exprsInline) ++ " inline expressions"
  resultsInline <- evalExpr exprsInline
  putStrLn $ "[=] inserting " ++ show (length resultsBlock, length resultsInline) ++ " (block/inline) results"
  let (evaluated, remaining) = CMS.runState (walkM replaceExprs doc) (resultsBlock, resultsInline)
  case remaining of
    ([], []) ->
      let withoutCode = walk eraseCodeBlocks evaluated in
      return $ writeMarkdown withoutCode
    _ ->
      fail $ "extra results: " ++ groom remaining


phaseEval :: String -> IO String
phaseEval text = do
  putStrLn $ "[=] parsing " ++ show (length text) ++ " characters"
  case Pandoc.readLaTeX readerOpts text of
    Left parseError -> fail $ groom parseError
    Right doc       -> processDoc doc


readMarkdown :: String -> Either PandocError Pandoc.Pandoc
readMarkdown =
  Pandoc.readMarkdown Pandoc.def
    { Pandoc.readerExtensions    = semdocExtensions
    , Pandoc.readerColumns       = 79
    }


writeMarkdown :: Pandoc.Pandoc -> String
writeMarkdown =
  Pandoc.writeMarkdown Pandoc.def
    { Pandoc.writerExtensions    = semdocExtensions
    , Pandoc.writerColumns       = 79
    , Pandoc.writerSetextHeaders = False
    , Pandoc.writerTeXLigatures  = False
    }
