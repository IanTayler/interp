-- | Implements the lexer/tokenizer part of interp.
-- Should mostly be used by calling the tokenize function.
{-# LANGUAGE OverloadedStrings #-}

module Data.Interp.Tokenizer where

import           Control.Applicative
import           Data.Either
import           Data.Void

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Interp.Operators

type Parser = P.Parsec Void Text

-- | Internal representation of a token.
data Token
  = OperatorTok { opArity :: Int
                , opId    :: OperatorID }
  | NumberTok Double
  | NameTok Text
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

getNumberToken :: Double -> Token
getNumberToken = NumberTok

getNameToken :: Text -> Token
getNameToken s = NameTok s

-- | Create a Token from a string. Assume it's an operator.
getOpToken :: Text -> Token
getOpToken stringRep
  | stringRep == minusOp = OperatorTok 2 MinusOp
  | stringRep == plusOp = OperatorTok 2 PlusOp
  | stringRep == prodOp = OperatorTok 2 ProdOp
  | stringRep == divOp = OperatorTok 2 DivOp
  | stringRep == createAssignOp = OperatorTok 2 CreateAssignOp
  | stringRep == assignOp = OperatorTok 2 AssignOp
  | stringRep == returnOp = OperatorTok 2 ReturnOp

-- | Whitespace and comment parser/ignorer.
spaceSkipP :: Parser ()
spaceSkipP =
  L.space P.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Wrapper around other parsers that then skips whitespaces/comments.
lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme spaceSkipP

-- | Parses a specific string, then skips whitespaces/comments.
symbolP :: Text -> Parser Text
symbolP = L.symbol spaceSkipP

-- | Parser for numbers.
-- All numbers get represented as Doubles internally.
numberP :: Parser Token
numberP =
  getNumberToken <$>
  (P.try (lexemeP L.float) <|> (fromIntegral <$> lexemeP L.decimal))

-- | Parser for alphabetic names.
nameP :: Parser Token
nameP = getNameToken . T.pack <$> P.someTill P.letterChar P.space1

-- | Parser for operators.
-- Will return operators with binary-arity IDs always. The parser will
-- correct the cases where "-" and "+" are actually unary later.
operatorP :: Parser Token
operatorP =
  fmap
    getOpToken
    (foldr
       (<|>)
       (symbolP plusOp)
       (map symbolP [minusOp, prodOp, divOp, createAssignOp, assignOp]))

fullKeyword :: Text -> Parser Text
fullKeyword inp = lexemeP $ P.string inp <* P.notFollowedBy P.alphaNumChar

keywordP :: Parser Token
keywordP = fmap getOpToken (fullKeyword returnOp)

-- | Parser for parenthesis.
parenP :: Parser Token
parenP =
  fmap (const OpenParenTok) (symbolP "(") <|>
  fmap (const CloseParenTok) (symbolP ")")

-- | General parser for tokens.
tokenP :: Parser Token
tokenP = numberP <|> keywordP <|> operatorP <|> parenP <|> nameP

tokenListP :: Parser [Token]
tokenListP = P.manyTill tokenP P.eof

-- | Gets the next ReadP-type tuple result using tokenP.
-- nextToken :: Text -> [(Token, String)]
-- nextToken = readP_to_S tokenP
-- | Helper for tokenizing by recursing over nextToken.
-- tokenizeStep :: [(Token, String)] -> [Token]
-- tokenizeStep [] = []
-- tokenizeStep ((token, restString):xs) =
--   token : tokenizeStep (nextToken restString)
-- | Separate an input string into its tokens.
tokenize :: Text -> [Token]
tokenize inp = fromRight [] $ P.runParser tokenListP "" inp
