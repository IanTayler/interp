{-# LANGUAGE OverloadedStrings #-}

module Data.Interp.Parser
  ( parse
  ) where

import           Data.Maybe
import           Debug.Trace

import           Data.Text               (Text)
import qualified Data.Text               as T

import           Data.Interp.InterTree
import           Data.Interp.Operators
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

-- | Function for getting a AST InterTree from each line.
-- If the line ends in "(" or ",", we will look at the next one
-- as well.
parseLines :: Text -> [Maybe InterTree]
parseLines "" = []
parseLines inp =
  if null firstTokens
    then []
    else parseExpr firstTokens : parseLines rest
  where
    (firstChunk, rest) = getParsingChunk inp
    firstTokens = tokenize firstChunk

-- | Check if a tree is a block-ending mark.
isBlockEnd :: InterTree -> Bool
isBlockEnd (InterTree (OperatorTok _ EndOp) _)  = True
isBlockEnd (InterTree (OperatorTok _ ElseOp) _) = True
isBlockEnd tree                                 = False

-- | Check if a tree is a block-starting mark.
isBlockStart :: InterTree -> Bool
isBlockStart (InterTree (OperatorTok _ IfOp) _)   = True
isBlockStart (InterTree (OperatorTok _ ForOp) _)  = True
isBlockStart (InterTree (OperatorTok _ ElseOp) _) = True
isBlockStart tree                                 = False

-- | Form a block. Push back trees until you find a block-beginning mark.
unrollBlockStack :: InterTree -> [(InterTree, Bool)] -> [(InterTree, Bool)]
unrollBlockStack block [] = [(block, False)]
unrollBlockStack block ((top, True):rest) =
  (treeAddChild top block, False) : rest
unrollBlockStack block ((top, stopMark):rest) =
  unrollBlockStack (treePreppendChild block top) rest

-- | Form blocks from line-based ASTs.
-- The trees in the stack are marked with True if they start a block
-- that hasn't yet formed. That way, when we find an "end" line we
-- unroll until we find a True-marked tree.
makeBlocks :: [Maybe InterTree] -> [(InterTree, Bool)] -> Maybe [InterTree]
makeBlocks [] stack = Just $ map fst stack
makeBlocks (fstTree:rest) stack
  | null fstTree = trace (show (fstTree : rest)) Nothing
  -- Things like "else" both end a block and start a new one.
  -- We treat them as a special case.
  | isBlockEnd justFstTree && isBlockStart justFstTree =
    makeBlocks
      rest
      ((justFstTree, True) :
       unrollBlockStack (InterTree BlockTok emptyChildren) stack)
  | isBlockEnd justFstTree =
    makeBlocks rest (unrollBlockStack (InterTree BlockTok emptyChildren) stack)
  | isBlockStart justFstTree = makeBlocks rest ((justFstTree, True) : stack)
  | otherwise = makeBlocks rest ((justFstTree, False) : stack)
  where
    justFstTree = fromJust fstTree

parse :: Text -> Maybe [InterTree]
parse inp = makeBlocks (parseLines inp) []
