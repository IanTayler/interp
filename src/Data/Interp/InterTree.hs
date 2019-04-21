module Data.Interp.InterTree where

import           Data.Interp.Operators
import           Data.Interp.Tokenizer

-- | AST result of parsing text.
data InterTree = InterTree
  { treeToken    :: Token
  , treeChildren :: [InterTree]
  }

-- | Add one child to an existing tree.
-- Children are added to the left for pitiful time savings.
treeAddChild :: InterTree -> InterTree -> InterTree
treeAddChild tree child = InterTree (treeToken tree) (child : treeChildren tree)

-- | Operator precedence for a tree (-1 for non-operators)
treeOpPrec (InterTree (OperatorTok _ id) _) = opPrec id
treeOpPrec _                                = -1

instance Show InterTree where
  show tree
    | null children = show tok
    | otherwise = "(" ++ show tok ++ " " ++ show children ++ ")"
    where
      (InterTree tok children) = tree
  showList []            = (++ "")
  showList [tree]        = \s -> s ++ show tree
  showList (tree:others) = \s -> show tree ++ " " ++ show others
