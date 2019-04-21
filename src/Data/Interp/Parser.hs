module Data.Interp.Parser
  ( parse
  ) where

import           Data.Interp.InterTree
import           Data.Interp.Parser.Expr
import           Data.Interp.Tokenizer

-- | Function for getting an AST InterTree from tokenized text.
parse :: [Token] -> Maybe InterTree
parse = parseExpr
