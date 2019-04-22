{-# LANGUAGE OverloadedStrings #-}

module Data.Interp.Parser
  ( parse
  ) where

import           Debug.Trace

import           Data.Text               (Text)
import qualified Data.Text               as T

import           Data.Interp.InterTree
import           Data.Interp.Parser.Expr
import           Data.Interp.Tokenizer

nextLine :: Text -> (Text, Text)
nextLine inp = (firstLine, T.stripStart rest)
  where
    (firstLine, rest) = T.breakOn "\n" inp

getParsingChunk :: Text -> (Text, Text)
getParsingChunk inp =
  if "," `T.isSuffixOf` firstLine || "(" `T.isSuffixOf` firstLine
    then (T.concat [firstLine, secondLine], secondRest)
    else (firstLine, rest)
  where
    (firstLine, rest) = nextLine inp
    (secondLine, secondRest) = getParsingChunk rest

-- | Function for getting an AST InterTree from text.
parse :: Text -> [Maybe InterTree]
parse "" = []
parse inp =
  if null firstTokens
    then []
    else parseExpr firstTokens : parse rest
  where
    (firstChunk, rest) = getParsingChunk inp
    firstTokens = tokenize firstChunk
