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
