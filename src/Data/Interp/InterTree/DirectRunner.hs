-- | Run InterTree AST directly, interpreted.
module Data.Interp.InterTree.DirectRunner where

import           Data.Maybe

import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Data.Interp.InterTree
import           Data.Interp.Operators
import           Data.Interp.Tokenizer

-- | Representing the value of a variable.
data InterValue
  = InterDouble Double
  | InterVoid
  deriving (Show)

doubleVal :: InterValue -> Double
doubleVal (InterDouble val) = val
doubleVal other             = 0.0

binaryDoubleOp ::
     (Double -> Double -> Double) -> InterValue -> InterValue -> InterValue
binaryDoubleOp op left right =
  InterDouble (op (doubleVal left) (doubleVal right))

unaryDoubleOp :: (Double -> Double) -> InterValue -> InterValue
unaryDoubleOp op operand = InterDouble (op $ doubleVal operand)

-- | Current state of the running program.
data InterState = InterState
  { varMap      :: Map.Map Text InterValue
  , parentState :: Maybe InterState
  }

initState = InterState Map.empty Nothing

evalTree :: InterTree -> InterState -> (InterValue, InterState)
evalTree (InterTree (OperatorTok _ opId) children) state
  | opId == PlusOp = (binaryDoubleOp (+) leftVal rightVal, rightState)
  | opId == MinusOp = (binaryDoubleOp (-) leftVal rightVal, rightState)
  | opId == ProdOp = (binaryDoubleOp (*) leftVal rightVal, rightState)
  | opId == DivOp = (binaryDoubleOp (/) leftVal rightVal, rightState)
  | opId == UnMinusOp = (unaryDoubleOp (0 -) leftVal, leftState)
  | opId == UnPlusOp = (leftVal, leftState)
  where
    (leftVal, leftState) =
      evalTree (fromJust $ childrenChildAt children 0) state
    (rightVal, rightState) =
      evalTree (fromJust $ childrenChildAt children 1) leftState
evalTree (InterTree (NumberTok val) _) state = (InterDouble val, state)
