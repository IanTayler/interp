-- | Parser for expressions and statements (despite its name)
module Data.Interp.Parser.Expr
  ( parseExpr
  ) where

import           Debug.Trace

import           Data.Interp.InterTree
import           Data.Interp.Operators
import           Data.Interp.Tokenizer

-- | True if Token is operator.
isOperator :: Token -> Bool
isOperator (OperatorTok _ _) = True
isOperator _                 = False

-- | True if Token is a Montague-style entity.
isEntity :: Token -> Bool
isEntity (NumberTok _) = True
isEntity (NameTok _)   = True
isEntity _             = False

-- | True if Token is an opening parenthesis.
isOpenParen :: Token -> Bool
isOpenParen OpenParenTok = True
isOpenParen _            = False

-- | True if Token is a closing parenthesis.
isCloseParen :: Token -> Bool
isCloseParen CloseParenTok = True
isCloseParen _             = False

-- | True if Token is a parenthesis.
isParen :: Token -> Bool
isParen tok = isOpenParen tok || isCloseParen tok

-- | True if Token is ignored if at the end of a line.
isFinalIgnore :: Token -> Bool
isFinalIgnore (OperatorTok _ ThenOp) = True
isFinalIgnore (OperatorTok _ DoOp)   = True
isFinalIgnore tok                    = False

-- | True if Token is treated as zero arity if at the end of a line.
isFinalZero :: Token -> Bool
isFinalZero (OperatorTok _ EndOp)  = True
isFinalZero (OperatorTok _ ElseOp) = True
isFinalZero tok                    = False

-- | True if Nothing, is operator or open parenthesis.
nullOpOrOpenParen :: Maybe Token -> Bool
nullOpOrOpenParen Nothing                  = True
nullOpOrOpenParen (Just (OperatorTok _ _)) = True
nullOpOrOpenParen (Just OpenParenTok)      = True
nullOpOrOpenParen _                        = False

unaryFromTok (OperatorTok _ id) =
  (InterTree (OperatorTok 1 (unaryEquiv id)) [], 1)

nonUnary :: Token -> (InterTree, Int)
nonUnary (NumberTok v) = (InterTree (NumberTok v) [], 0)
nonUnary (NameTok n) = (InterTree (NameTok n) [], 0)
nonUnary (OperatorTok arity opId) =
  (InterTree (OperatorTok arity opId) [], arity)
nonUnary tok = (InterTree tok [], 2)

-- | Detect unary operators. Return list of (tree, isUnary) pairs.
-- The returned list will have the same tokens in the same order,
-- other than the fact that it will merge unary operators applied
-- to numbers with the numbers so that ["-", "8", "*", "9"] will
-- become "[(- 8), *, 9].
unaryDetectionPass :: Maybe Token -> [Token] -> [(InterTree, Int)]
unaryDetectionPass _ [] = []
unaryDetectionPass prev (tok:toks)
  | isOperator tok && nullOpOrOpenParen prev =
    unaryFromTok tok : unaryDetectionPass (Just tok) toks
  | otherwise = nonUnary tok : unaryDetectionPass (Just tok) toks
  where
    nextTok = head toks

-- | Auxiliary function checking that a list has at least n elems.
atLeast n []     = n <= 0
atLeast n (x:xs) = atLeast (n - 1) xs

traceInStackPass toks stack =
  trace ("TOKS: " ++ show toks ++ " STACK: " ++ show stack) Nothing

-- | Rolls back the stack making each tree a child of the next.
-- Stops when it runs into an open parenthesis.
unrollStack :: [(InterTree, Int)] -> [(InterTree, Int)]
unrollStack [] = []
unrollStack [tree] = [tree]
unrollStack ((topTree, topAr):(secTree, secAr):restStack)
  | isOpenParen (treeToken secTree) = (topTree, topAr) : restStack
  | otherwise =
    unrollStack ((treeAddChild secTree topTree, secAr - 1) : restStack)

-- | Shift-reduce parser pass. This is where most of the magic happens.
stackPass :: [(InterTree, Int)] -> [(InterTree, Int)] -> Maybe InterTree
stackPass [] [] = Nothing
stackPass [] [(tree, 0)] = Just tree
stackPass [] [(tree, _)] = Nothing
stackPass [] stack = stackPass [] (unrollStack stack)
stackPass ((fstTree, fstArity):other) [] =
  if null other && (isFinalZero $ treeToken fstTree)
    then stackPass [] [(fstTree, 0)]
    else stackPass other [(fstTree, fstArity)]
stackPass ((fstTree, 2):other) ((topTree, 0):restStack)
  | null other && isFinalIgnore fstTreeToken =
    stackPass [] ((topTree, 0) : restStack)
  | isOpenParen fstTreeToken =
    stackPass other ((fstTree, 2) : (topTree, 0) : restStack)
  | isCloseParen fstTreeToken =
    stackPass other (unrollStack ((topTree, 0) : restStack))
  | null restStack ||
      isParen (treeToken secTopTree) || secTopTreePrec < fstTreePrec =
    stackPass other ((treeAddChild fstTree topTree, 1) : restStack)
  | otherwise =
    stackPass
      ((fstTree, 2) : other)
      ((treeAddChild secTopTree topTree, secTopArity - 1) : tail restStack)
  where
    fstTreeToken = treeToken fstTree
    (secTopTree, secTopArity) = head restStack
    secTopTreePrec = treeOpPrec secTopTree
    fstTreePrec = treeOpPrec fstTree
-- Here we have topArity != 0
stackPass ((fstTree, 2):other) ((topTree, topArity):restStack)
  | null other && isFinalIgnore fstTreeToken =
    stackPass [] ((topTree, topArity) : restStack)
  | isOpenParen fstTreeToken =
    stackPass other ((fstTree, 2) : (topTree, topArity) : restStack)
  | isCloseParen fstTreeToken =
    stackPass other (unrollStack ((topTree, topArity) : restStack))
  | otherwise =
    traceInStackPass ((fstTree, 2) : other) ((topTree, topArity) : restStack)
  where
    fstTreeToken = treeToken fstTree
-- Assuming arity < 2
stackPass ((fstTree, arity):other) stack =
  stackPass other ((fstTree, arity) : stack)

-- | Function for getting an AST InterTree from tokenized text representing an expression or a statement.
parseExpr :: [Token] -> Maybe InterTree
parseExpr toks = stackPass (unaryDetectionPass Nothing toks) []
