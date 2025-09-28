module Parsing where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

data Expr =
      Number Double
    | BinaryOpperation BinaryOpperator Expr Expr
    | UnaryOpperation UnaryOpperator Expr
    deriving Show

data BinaryOpperator = Plus | Minus | Multiply | Divide deriving Show

data UnaryOpperator = Negation deriving Show

def :: LanguageDef ()
def = emptyDef {
    opStart = oneOf "+-*/",
    opLetter = oneOf "",
    reservedOpNames = ["+", "-", "*", "/"]
}

TokenParser {
    naturalOrFloat  = m_naturalOrFloat,
    parens = m_parens,
    reservedOp = m_reservedOp,
    whiteSpace = m_whiteSpace
} = makeTokenParser def

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

table :: [[Operator String () Identity Expr]]
table = [
    [Prefix (m_reservedOp "-" >> return (UnaryOpperation Negation))           ],

    [Infix  (m_reservedOp "*" >> return (BinaryOpperation Multiply)) AssocLeft,
     Infix  (m_reservedOp "/" >> return (BinaryOpperation Divide))   AssocLeft],

    [Infix  (m_reservedOp "+" >> return (BinaryOpperation Plus))     AssocLeft,
     Infix  (m_reservedOp "-" >> return (BinaryOpperation Minus))    AssocLeft]]

number :: Parser Expr
number = f <$> m_naturalOrFloat
    where
        f :: Either Integer Double -> Expr
        f (Left i) = Number (fromIntegral i)
        f (Right d)  = Number d

term :: Parser Expr
term = m_parens expression
    <|> number

parse_source :: String -> Either ParseError Expr
parse_source x = parse expression "" x


