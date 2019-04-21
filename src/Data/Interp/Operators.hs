-- | Mostly constants for operators.
module Data.Interp.Operators where

minusOp = "-"

plusOp = "+"

prodOp = "*"

divOp = "/"

createAssignOp = ":="

assignOp = "="

returnOp = "return"

data OperatorID
  = MinusOp
  | PlusOp
  | UnMinusOp
  | UnPlusOp
  | ProdOp
  | DivOp
  | CreateAssignOp
  | AssignOp
  | ReturnOp
  deriving (Show, Eq)

-- | Convert an operatorID to its unary equivalent, if it exists.
unaryEquiv :: OperatorID -> OperatorID
unaryEquiv MinusOp = UnMinusOp
unaryEquiv PlusOp  = UnPlusOp
unaryEquiv op      = op -- Catch all case.

-- | Gets the precedence value for an operator ID.
opPrec :: OperatorID -> Int
opPrec op
  | op `elem` [ReturnOp] = 3
  | op `elem` [CreateAssignOp, AssignOp] = 6
  | op `elem` [MinusOp, PlusOp] = 12
  | op `elem` [ProdOp, DivOp] = 24
  | op `elem` [UnMinusOp, UnPlusOp] = 36
