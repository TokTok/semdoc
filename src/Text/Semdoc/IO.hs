module Text.Semdoc.IO where

import           Control.Applicative       ((<$>))
import           Text.Groom                (groom)
import           Text.Pandoc.Readers.LaTeX (handleIncludes)

import           Text.Semdoc.Processor


phaseRead :: FilePath -> IO String
phaseRead file = do
  putStrLn $ "[=] loading input: " ++ file
  contents <- readFile file >>= handleIncludes
  case contents of
    Left includeError -> fail $ groom includeError
    Right text        -> return text


phaseWrite :: FilePath -> String -> IO ()
phaseWrite file md = do
  putStrLn $ "[=] writing output: " ++ file
  case writeMarkdown <$> readMarkdown md of
    Left parseError -> fail $ groom parseError
    Right formatted -> writeFile file $ "% The Tox Reference\n\n" ++ formatted ++ "\n"
