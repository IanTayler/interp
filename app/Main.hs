module Main where

import           System.IO

import qualified Data.Text             as T

import           Data.Interp.Parser
import           Data.Interp.Tokenizer

main :: IO ()
main = do
  content <- getContents
  print $ parse $ T.pack content
