{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           System.Environment (getArgs)

import           Text.Semdoc


main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> phaseRead input >>= phaseEval >>= phaseWrite output
    _               -> fail "Usage: semdoc <input.lhs> <output.md>"
