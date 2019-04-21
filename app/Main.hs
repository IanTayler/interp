module Main where

import           System.IO

import           Data.Interp.Parser
import           Data.Interp.Tokenizer

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  line <- getLine
  print $ parse (tokenize line)
  main
