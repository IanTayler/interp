-- | Implements the lexer/tokenizer part of interp.
-- Should mostly be used by calling the tokenize function.
module Data.Interp.Tokenizer where

import           Control.Applicative
import           Text.ParserCombinators.ReadP

import           Data.Interp.Operators

-- | Internal representation of a token.
data Token
  = OperatorTok { opArity :: Int
                , opId    :: OperatorID }
  | NumberTok Double
  | NameTok String
  | OpenParenTok
  | CloseParenTok

instance Show Token where
  show tok =
    case tok of
      OperatorTok _ id -> show id
      NumberTok num    -> show num
      NameTok name     -> show name
      OpenParenTok     -> "OpenParen"
      CloseParenTok    -> "CloseParen"

getNumberToken :: String -> Token
getNumberToken stringRep = NumberTok (read stringRep)

getNameToken :: String -> Token
getNameToken = NameTok

-- | Create a Token from a string. Assume it's an operator.
getOpToken :: String -> Token
getOpToken stringRep
  | stringRep == minusOp = OperatorTok 2 MinusOp
  | stringRep == plusOp = OperatorTok 2 PlusOp
  | stringRep == prodOp = OperatorTok 2 ProdOp
  | stringRep == divOp = OperatorTok 2 DivOp
  | stringRep == createAssignOp = OperatorTok 2 CreateAssignOp
  | stringRep == assignOp = OperatorTok 2 AssignOp

-- | Helper that checks char is a digit.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- | Parser for numbers.
numberP :: ReadP Token
numberP = fmap getNumberToken (munch1 isDigit)

-- | Helper that checks char is alphabetic.
isAlpha :: Char -> Bool
isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'

nameP = fmap getNameToken (munch1 isAlpha)

-- | Parser for operators.
-- Will return operators with binary-arity IDs always. The parser will
-- correct the cases where "-" and "+" are actually unary later.
operatorP :: ReadP Token
operatorP =
  fmap
    getOpToken
    (foldr
       (<|>)
       pfail
       (map string [plusOp, minusOp, prodOp, divOp, createAssignOp, assignOp]))

-- | Parser for parenthesis.
parenP :: ReadP Token
parenP =
  fmap (const OpenParenTok) (string "(") <|>
  fmap (const CloseParenTok) (string ")")

-- | General parser for tokens.
tokenP :: ReadP Token
tokenP = do
  skipSpaces
  numberP <|> operatorP <|> parenP <|> nameP

-- | Gets the next ReadP-type tuple result using tokenP.
nextToken :: String -> [(Token, String)]
nextToken = readP_to_S tokenP

-- | Helper for tokenizing by recursing over nextToken.
tokenizeStep :: [(Token, String)] -> [Token]
tokenizeStep [] = []
tokenizeStep ((token, restString):xs) =
  token : tokenizeStep (nextToken restString)

-- | Separate an input string into its tokens.
tokenize :: String -> [Token]
tokenize = tokenizeStep . nextToken
