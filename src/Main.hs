{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Control.Monad.State.Strict as CMS
import qualified Data.Set                   as Set
import           System.Environment         (getArgs)
import           Text.Groom                 (groom)
import           Text.Pandoc                (Block (..), Format (..),
                                             Inline (..))
import qualified Text.Pandoc                as Pandoc
import           Text.Pandoc.Error          (PandocError)
import           Text.Pandoc.Readers.LaTeX  (handleIncludes)
import           Text.Pandoc.Walk           (walk, walkM)
import           Text.Regex.TDFA            ((=~))

import           EvalExpr                   (EvalInput (EvalInput), evalExpr)


readerOpts :: Pandoc.ReaderOptions
readerOpts = Pandoc.def
  { Pandoc.readerExtensions = Set.fromList [Pandoc.Ext_literate_haskell]
  , Pandoc.readerParseRaw = True
  }


extractExprsInline :: Inline -> CMS.State ([EvalInput], String) Inline
extractExprsInline i =
  case i of
    RawInline (Format "latex") ('\\':'s':'h':'o':'w':'{':expr) -> add expr
    Code ("", [], []) ('\\':'s':'h':'o':'w':'{':expr)          -> add expr
    _                                                          -> return i

  where
    add :: String -> CMS.State ([EvalInput], String) Inline
    add expr = do
      (inputs, modName) <- CMS.get
      CMS.put (EvalInput modName ("it = " ++ init expr) : inputs, modName)
      return i


extractExprs :: Block -> CMS.State ([EvalInput], String) Block
extractExprs i =
  case i of
    CodeBlock ("", ["sourceCode", "literate", "haskell"], []) code ->
      case code =~ "module ([A-Z][A-Za-z0-9_.]+)" of
        [[_, modName]] -> do
          (inputs, _) <- CMS.get
          CMS.put (inputs, modName)
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


doReplace :: CMS.State [a] a
doReplace = do
  results <- CMS.get
  case results of
    [] -> fail "insufficient results"
    r:rs -> do
      CMS.put rs
      return r


replaceExprs :: Inline -> CMS.State [Inline] Inline
replaceExprs i =
  case i of
    RawInline (Format "latex") ('\\':'s':'h':'o':'w':'{':_) -> doReplace
    Code ("", [], []) ('\\':'s':'h':'o':'w':'{':_)          -> doReplace
    _                                                       -> return i

eraseCodeBlocks :: Block -> Block
eraseCodeBlocks (CodeBlock ("", ["sourceCode", "literate", "haskell"], []) _) =
  Null
eraseCodeBlocks x = x


phaseRead :: FilePath -> IO String
phaseRead file = do
  contents <- readFile file >>= handleIncludes
  case contents of
    Left includeError -> fail $ groom includeError
    Right text        -> return text


semdocExtensions :: Set.Set Pandoc.Extension
semdocExtensions =
  Set.insert Pandoc.Ext_tex_math_dollars $
    foldl (flip Set.delete) Pandoc.githubMarkdownExtensions
      [ Pandoc.Ext_hard_line_breaks
      , Pandoc.Ext_native_spans
      , Pandoc.Ext_raw_html
      , Pandoc.Ext_tex_math_single_backslash
      ]


writeMarkdown :: Pandoc.Pandoc -> String
writeMarkdown =
  Pandoc.writeMarkdown Pandoc.def
    { Pandoc.writerExtensions    = semdocExtensions
    , Pandoc.writerColumns       = 79
    , Pandoc.writerSetextHeaders = False
    , Pandoc.writerTeXLigatures  = False
    }


readMarkdown :: String -> Either PandocError Pandoc.Pandoc
readMarkdown =
  Pandoc.readMarkdown Pandoc.def
    { Pandoc.readerExtensions    = semdocExtensions
    , Pandoc.readerColumns       = 79
    }


processDoc :: Pandoc.Pandoc -> IO String
processDoc doc@(Pandoc.Pandoc _ parsed) = do
  putStrLn $ "[=] processing " ++ show (length parsed) ++ " toplevel blocks"
  putStrLn $ groom parsed
  let exprs = reverse . fst . snd . CMS.runState (walkM extractExprs doc) $ ([], "Prelude")
  putStrLn $ "[=] evaluating " ++ show (length exprs) ++ " expressions"
  putStrLn $ groom exprs
  results <- evalExpr exprs
  putStrLn $ "[=] inserting " ++ show (length results) ++ " results"
  let (evaluated, remaining) = CMS.runState (walkM replaceExprs doc) results
  case remaining of
    [] ->
      let withoutCode = walk eraseCodeBlocks evaluated in
      return $ writeMarkdown withoutCode
    _ ->
      fail $ "extra results: " ++ groom remaining


processFile :: FilePath -> IO String
processFile file = do
  putStrLn $ "[=] loading input: " ++ file
  text <- phaseRead file
  putStrLn $ "[=] parsing " ++ show (length text) ++ " characters"
  case Pandoc.readLaTeX readerOpts text of
    Left parseError -> fail $ groom parseError
    Right doc       -> processDoc doc


phasePrint :: FilePath -> String -> IO ()
phasePrint file md = do
  putStrLn $ "[=] writing output: " ++ file
  case writeMarkdown <$> readMarkdown md of
    Left parseError -> fail $ groom parseError
    Right formatted -> writeFile file $ "% The Tox Reference\n\n" ++ formatted ++ "\n"


main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> processFile input >>= phasePrint output
    _               -> fail "Usage: semdoc <input.lhs> <output.md>"
