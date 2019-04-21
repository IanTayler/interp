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
  | OpenParenTok
  | CloseParenTok

instance Show Token where
  show tok =
    case tok of
      OperatorTok _ id -> show id
      NumberTok num    -> show num
      OpenParenTok     -> "OpenParen"
      CloseParenTok    -> "CloseParen"

getNumberToken :: String -> Token
getNumberToken stringRep = NumberTok (read stringRep)

-- | Create a Token from a string. Assume it's an operator.
getOpToken :: String -> Token
getOpToken stringRep
  | stringRep == minusOp = OperatorTok 2 MinusOp
  | stringRep == plusOp = OperatorTok 2 PlusOp
  | stringRep == prodOp = OperatorTok 2 ProdOp
  | stringRep == divOp = OperatorTok 2 DivOp

-- | Helper that checks char is a digit.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- | Parser for numbers.
numberP :: ReadP Token
numberP = fmap getNumberToken (munch1 isDigit)

-- | Parser for operators.
-- Will return operators with binary-arity IDs always. The parser will
-- correct the cases where "-" and "+" are actually unary later.
operatorP :: ReadP Token
operatorP =
  fmap
    getOpToken
    (string plusOp <|> string minusOp <|> string prodOp <|> string divOp)

-- | Parser for parenthesis.
parenP :: ReadP Token
parenP =
  fmap (const OpenParenTok) (string "(") <|>
  fmap (const CloseParenTok) (string ")")

-- | General parser for tokens.
tokenP :: ReadP Token
tokenP = do
  skipSpaces
  numberP <|> operatorP <|> parenP

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
