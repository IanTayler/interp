-- | Mostly constants for operators.
{-# LANGUAGE OverloadedStrings #-}

module Data.Interp.Operators where

import           Data.Text (Text)

ltEqOp = ">=" :: Text

gtEqOp = "<=" :: Text

eqOp = "==" :: Text

ltOp = "<" :: Text

gtOp = ">" :: Text

minusOp = "-" :: Text

plusOp = "+" :: Text

prodOp = "*" :: Text

divOp = "/" :: Text

createAssignOp = ":=" :: Text

assignOp = "=" :: Text

semicolonOp = ";" :: Text

commaOp = "," :: Text

retTypeOp = "->" :: Text

typeSpecOp = ":" :: Text

funcOp = "func" :: Text

returnOp = "return" :: Text

deferOp = "defer" :: Text

ifOp = "if" :: Text

thenOp = "then" :: Text

elseOp = "else" :: Text

forOp = "for" :: Text

doOp = "do" :: Text

endOp = "end" :: Text

andOp = "and" :: Text

orOp = "or" :: Text

notOp = "not" :: Text

data OperatorID
  = LtEqOp
  | GtEqOp
  | EqOp
  | LtOp
  | GtOp
  | MinusOp
  | PlusOp
  | UnMinusOp
  | UnPlusOp
  | ProdOp
  | DivOp
  | CreateAssignOp
  | AssignOp
  | SemicolonOp
  | CommaOp
  | ApplicationOp
  | RetTypeOp
  | TypeSpecOp
  | FuncOp
  | ReturnOp
  | DeferOp
  | IfOp
  | ThenOp
  | ElseOp
  | ForOp
  | DoOp
  | EndOp
  | AndOp
  | OrOp
  | NotOp
  deriving (Show, Eq)

-- | Convert an operatorID to its unary equivalent, if it exists.
unaryEquiv :: OperatorID -> OperatorID
unaryEquiv MinusOp = UnMinusOp
unaryEquiv PlusOp  = UnPlusOp
unaryEquiv op      = op -- Catch all case.

-- | Gets the precedence value for an operator ID.
opPrec :: OperatorID -> Int
opPrec op
  | op `elem`
      [ReturnOp, DeferOp, IfOp, ThenOp, ElseOp, ForOp, DoOp, EndOp, FuncOp] = 3
  | op == SemicolonOp = 4
  | op == CommaOp = 5
  | op `elem` [CreateAssignOp, AssignOp] = 6
  | op == RetTypeOp = 8
  | op `elem` [AndOp, OrOp, NotOp] = 9
  | op `elem` [LtOp, GtOp, LtEqOp, GtEqOp, EqOp] = 10
  | op `elem` [MinusOp, PlusOp] = 12
  | op `elem` [ProdOp, DivOp] = 24
  | op `elem` [UnMinusOp, UnPlusOp] = 36
  | op `elem` [ApplicationOp, TypeSpecOp] = 48
