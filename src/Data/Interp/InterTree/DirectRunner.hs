{-# LANGUAGE OverloadedStrings #-}

-- | Run InterTree AST directly, interpreted.
module Data.Interp.InterTree.DirectRunner where

import qualified Data.List             as List
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
  | EvalError Text
  deriving (Show)

hasEvalError :: InterValue -> Bool
hasEvalError (EvalError _) = True
hasEvalError other         = False

checkErrors ::
     [InterValue] -> (InterValue, InterState) -> (InterValue, InterState)
checkErrors checkVals expr =
  if any hasEvalError checkVals
    then (fromJust $ List.find hasEvalError checkVals, initState)
    else expr

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
  } deriving (Show)

initStateWithParent = InterState Map.empty

initState = initStateWithParent Nothing

-- | Set a new parent for an InterState.
stateChangeParent :: InterState -> Maybe InterState -> InterState
stateChangeParent (InterState m _) = InterState m

getLocalVar :: Text -> InterState -> Maybe InterValue
getLocalVar varName state = Map.lookup varName (varMap state)

createLocalVar :: Text -> InterValue -> InterState -> Maybe InterState
createLocalVar varName val state =
  if null $ getLocalVar varName state
    then Just $
         InterState (Map.insert varName val (varMap state)) (parentState state)
    else Nothing

setLocalVar :: Text -> InterValue -> InterState -> Maybe InterState
setLocalVar varName val state =
  if null $ getLocalVar varName state
    then Nothing
    else Just $
         InterState (Map.insert varName val (varMap state)) (parentState state)

findScopeAndCall ::
     InterState
  -> (InterState -> Maybe a)
  -> ((InterState, Maybe a) -> Maybe a)
  -> Maybe a
findScopeAndCall state func constructor
  | null localResult =
    if null parent
      then Nothing
      else constructor
             (state, findScopeAndCall (fromJust parent) func constructor)
  | otherwise = localResult
  where
    localResult = func state
    parent = parentState state

-- | Get variable in state or any ancestor state.
getVar :: Text -> InterState -> Maybe InterValue
getVar varName state = findScopeAndCall state (getLocalVar varName) snd

-- | Set existing variable in state or any ancestor state.
setVar :: Text -> InterValue -> InterState -> Maybe InterState
setVar name val state =
  findScopeAndCall
    state
    (setLocalVar name val)
    (Just . uncurry stateChangeParent)

assignWrapper ::
     (Text -> InterValue -> InterState -> Maybe InterState)
  -> TreeChildrenType
  -> InterState
  -> (InterValue, InterState)
assignWrapper assignOp children state
  | length children /= 2 =
    ( EvalError $
      T.concat
        [ "Wrong syntax when assigning to variable. Wrong number of children. "
        , T.pack $ show children
        ]
    , state)
  | null nameMaybe =
    (EvalError "Not a name to the left when assigning to a variable.", state)
  | otherwise =
    checkErrors
      [secondVal]
      (if null finalState
         then ( EvalError $
                T.concat
                  [ "Consistency error when assigning variable "
                  , name
                  , ". Check proper usage of = and :=."
                  ]
              , state)
         else (InterVoid, fromJust finalState))
  where
    first = fromJust $ childrenChildAt children 0
    nameMaybe = getTreeTokenName first
    name = fromJust nameMaybe
    second = fromJust $ childrenChildAt children 1
    (secondVal, secondState) = evalTree second state
    finalState = assignOp name secondVal secondState

evalCreateAssign :: TreeChildrenType -> InterState -> (InterValue, InterState)
evalCreateAssign = assignWrapper createLocalVar

evalAssign :: TreeChildrenType -> InterState -> (InterValue, InterState)
evalAssign = assignWrapper setVar

-- | Recurse over children in a block, running them all.
evalInBlock :: TreeChildrenType -> InterState -> (InterValue, InterState)
evalInBlock children state
  | null children = (InterVoid, state)
  | length children == 1 = evalTree child state
  | otherwise = evalInBlock nextChildren newState
  where
    child = childrenHead children
    nextChildren = childrenTail children
    (_, newState) = evalTree child state

evalTree :: InterTree -> InterState -> (InterValue, InterState)
evalTree (InterTree (OperatorTok _ opId) children) state
  -- Normal operators (*, and, >=, etc.)
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
  -- Assignment operators
  | opId == CreateAssignOp = evalCreateAssign children state
  | opId == AssignOp = evalAssign children state
  -- Special (i.e. weird) operators
  | opId == SemicolonOp = checkLeftRight (rightVal, rightState)
  | opId == IfOp =
    checkLeftRight
      (if boolVal leftVal
         then (rightVal, rightState)
         else (InterVoid, state) -- TODO: add else case.
       )
  where
    (leftVal, leftState) =
      evalTree (fromJust $ childrenChildAt children 0) state
    (rightVal, rightState) =
      evalTree (fromJust $ childrenChildAt children 1) leftState
    checkLeft = checkErrors [leftVal]
    checkLeftRight = checkErrors [leftVal, rightVal]
    binaryDouble x =
      checkLeftRight (binaryDoubleOp x leftVal rightVal, rightState)
    unaryDouble x = checkLeft (unaryDoubleOp x leftVal, leftState)
    binaryBool x = checkLeftRight (binaryBoolOp x leftVal rightVal, rightState)
    unaryBool x = checkLeft (unaryBoolOp x leftVal, leftState)
    doubleComparison x =
      checkLeftRight (doubleComparisonOp x leftVal rightVal, rightState)
evalTree (InterTree (NumberTok val) _) state = (InterDouble val, state)
evalTree (InterTree (NameTok name) _) state =
  let varVal = getVar name state
   in if null varVal
        then ( EvalError $ T.concat ["Variable ", name, " does not exist."]
             , state)
        else (fromJust varVal, state)
evalTree (InterTree BlockTok children) state = (returnVal, newState)
  where
    blockState = initStateWithParent (Just state)
    (returnVal, newBlockState) = evalInBlock children blockState
    newState =
      if null $ parentState newBlockState
        then initState
        else fromJust $ parentState newBlockState
