module Data.Interp.InterTree where

import           Data.Sequence         ((<|), (|>))
import qualified Data.Sequence         as S

import           Data.Interp.Operators
import           Data.Interp.Tokenizer

-- | AST result of parsing text.
data InterTree = InterTree
  { treeToken    :: Token
  , treeChildren :: S.Seq InterTree
  }

-- | Alias for empty children in InterTree.
emptyChildren = S.empty

-- | Safe lookup by index in a tree's children.
-- Use childrenChildAt if you already have the children pseudolist.
-- Use childAt if you have the tree.
childAt :: InterTree -> Int -> Maybe InterTree
childAt (InterTree _ children) i = S.lookup i children

-- | Wrapper around Sequence.lookup for abstraction (arg order reversed).
-- Use childrenChildAt if you already have the children pseudolist.
-- Use childAt if you have the tree.
childrenChildAt :: S.Seq InterTree -> Int -> Maybe InterTree
childrenChildAt trees i = S.lookup i trees

-- | Add one child to the end of an existing tree.
treeAddChild :: InterTree -> InterTree -> InterTree
treeAddChild tree child =
  InterTree (treeToken tree) (treeChildren tree |> child)

-- | Add one child at the beginning of an existing tree's children.
treePreppendChild :: InterTree -> InterTree -> InterTree
treePreppendChild tree child =
  InterTree (treeToken tree) (child <| treeChildren tree)

-- | Operator precedence for a tree (-1 for non-operators)
treeOpPrec (InterTree (OperatorTok _ id) _) = opPrec id
treeOpPrec _                                = -1

instance Show InterTree where
  show tree
    | S.null children = show tok
    | otherwise = "(" ++ show tok ++ " " ++ show children ++ ")"
    where
      (InterTree tok children) = tree
