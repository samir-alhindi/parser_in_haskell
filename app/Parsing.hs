module Parsing where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

data AExpr =
      Number Double
    | BinaryOpperation ABinOpperator AExpr AExpr
    | UnaryOpperation AUnaryOpperator AExpr
    | Ternary AExpr AExpr AExpr
    deriving Show

data BExpr =
      BoolVal Bool
    | And BExpr BExpr
    | Or BExpr BExpr
    | Not BExpr
    deriving Show

data ABinOpperator = Plus | Minus | Multiply | Divide deriving Show

data AUnaryOpperator = Negation deriving Show

def :: LanguageDef ()
def = emptyDef {
    opStart = oneOf "+-*/",
    opLetter = oneOf "",
    reservedOpNames = ["+", "-", "*", "/", "and", "or", "not"],
    reservedNames  = ["true", "false", "and", "or", "not"]
}

TokenParser {
    naturalOrFloat  = m_naturalOrFloat,
    parens = m_parens,
    reservedOp = m_reservedOp,
    reserved  = m_reserved,
    whiteSpace = m_whiteSpace
} = makeTokenParser def

a_expression :: Parser AExpr
a_expression = buildExpressionParser a_table a_term <?> "math expression"

a_table :: [[Operator String () Identity AExpr]]
a_table = [
    [Prefix (m_reservedOp "-" >> return (UnaryOpperation Negation))           ],

    [Infix  (m_reservedOp "*" >> return (BinaryOpperation Multiply)) AssocLeft,
     Infix  (m_reservedOp "/" >> return (BinaryOpperation Divide))   AssocLeft],

    [Infix  (m_reservedOp "+" >> return (BinaryOpperation Plus))     AssocLeft,
     Infix  (m_reservedOp "-" >> return (BinaryOpperation Minus))    AssocLeft]]

number :: Parser AExpr
number = f <$> m_naturalOrFloat
    where
        f :: Either Integer Double -> AExpr
        f (Left i) = Number (fromIntegral i)
        f (Right d)  = Number d

a_term :: Parser AExpr
a_term = m_parens a_expression
    <|> number

b_expression :: Parser BExpr
b_expression = buildExpressionParser b_table b_term <?> "boolean expression"

b_table :: [[Operator String () Identity BExpr]]
b_table = [
    [Prefix (m_reservedOp "not" >> return (Not))           ],
    [Infix  (m_reservedOp "and" >> return (And)) AssocLeft],
    [Infix  (m_reservedOp "or" >> return (Or)) AssocLeft]
    ]

b_term :: Parser BExpr
b_term = m_parens b_expression
    <|>(m_reserved "true"   >> return (BoolVal True ))
    <|> (m_reserved "false" >> return (BoolVal False))

a_parse :: String -> Either ParseError AExpr
a_parse source = parse (m_whiteSpace >> a_expression) "" source

b_parse :: String -> Either ParseError BExpr
b_parse source = parse (m_whiteSpace >> b_expression) "" source