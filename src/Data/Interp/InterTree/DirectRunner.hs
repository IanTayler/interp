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
  | InterBool Bool
  | InterVoid
  deriving (Show)

doubleVal :: InterValue -> Double
doubleVal (InterDouble val) = val
doubleVal other             = 0.0

boolVal :: InterValue -> Bool
boolVal (InterBool val) = val
boolVal other           = True

binaryDoubleOp ::
     (Double -> Double -> Double) -> InterValue -> InterValue -> InterValue
binaryDoubleOp op left right =
  InterDouble (op (doubleVal left) (doubleVal right))

unaryDoubleOp :: (Double -> Double) -> InterValue -> InterValue
unaryDoubleOp op operand = InterDouble (op $ doubleVal operand)

binaryBoolOp :: (Bool -> Bool -> Bool) -> InterValue -> InterValue -> InterValue
binaryBoolOp op left right = InterBool (op (boolVal left) (boolVal right))

unaryBoolOp :: (Bool -> Bool) -> InterValue -> InterValue
unaryBoolOp op operand = InterBool (op (boolVal operand))

doubleComparisonOp ::
     (Double -> Double -> Bool) -> InterValue -> InterValue -> InterValue
doubleComparisonOp op left right =
  InterBool (op (doubleVal left) (doubleVal right))

-- | Current state of the running program.
data InterState = InterState
  { varMap      :: Map.Map Text InterValue
  , parentState :: Maybe InterState
  }

initState = InterState Map.empty Nothing

evalTree :: InterTree -> InterState -> (InterValue, InterState)
evalTree (InterTree (OperatorTok _ opId) children) state
  | opId == PlusOp = binaryDouble (+)
  | opId == MinusOp = binaryDouble (-)
  | opId == ProdOp = binaryDouble (*)
  | opId == DivOp = binaryDouble (/)
  | opId == UnMinusOp = unaryDouble negate
  | opId == UnPlusOp = unaryDouble id
  | opId == AndOp = binaryBool (&&)
  | opId == OrOp = binaryBool (||)
  | opId == NotOp = unaryBool not
  | opId == LtEqOp = doubleComparison (<=)
  | opId == LtOp = doubleComparison (<)
  | opId == GtEqOp = doubleComparison (>=)
  | opId == GtOp = doubleComparison (>)
  | opId == EqOp = doubleComparison (==)
  where
    (leftVal, leftState) =
      evalTree (fromJust $ childrenChildAt children 0) state
    (rightVal, rightState) =
      evalTree (fromJust $ childrenChildAt children 1) leftState
    binaryDouble x = (binaryDoubleOp x leftVal rightVal, rightState)
    unaryDouble x = (unaryDoubleOp x leftVal, leftState)
    binaryBool x = (binaryBoolOp x leftVal rightVal, rightState)
    unaryBool x = (unaryBoolOp x leftVal, leftState)
    doubleComparison x = (doubleComparisonOp x leftVal rightVal, rightState)
evalTree (InterTree (NumberTok val) _) state = (InterDouble val, state)
