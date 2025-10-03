module Parsing where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

data Stmt =
      Print Expr
    | If BExpr Stmt
    | IfElse BExpr Stmt Stmt
    | Seq [Stmt]
    deriving Show


data Expr =
      AE AExpr
    | BE BExpr
    | Ternary BExpr Expr Expr
    deriving Show

data AExpr =
      Number Double
    | BinaryOpperation ABinOpperator AExpr AExpr
    | UnaryOpperation AUnaryOpperator AExpr
    deriving Show

data BExpr =
      BoolVal Bool
    | And BExpr BExpr
    | Or BExpr BExpr
    | Not BExpr
    | RExpr AExpr AExpr ROpperator
    deriving Show

data ABinOpperator = Plus | Minus | Multiply | Divide deriving Show

data AUnaryOpperator = Negation deriving Show

data ROpperator = Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals deriving Show

program :: Parser Stmt
program = (m_parens program <|> statement_sequence) <* eof <?> "program"

statement_sequence :: Parser Stmt
statement_sequence = do
    list <- (many1 statement)
    if length list == 1
        then return (head list)
        else return (Seq list)

statement :: Parser Stmt
statement = try print' <|> try if_else <|> if' <?> "statement"

print' :: Parser Stmt
print' = do
    m_reserved "print"
    expre <- expression
    m_semi
    return (Print expre) <?> "print"

if_else :: Parser Stmt
if_else = do
    m_reserved "if"
    condition <- b_expression
    m_reserved "then"
    then_branch <- statement
    m_reserved "else"
    else_branch <- statement
    return (IfElse condition then_branch else_branch)

if' :: Parser Stmt
if' = do
    m_reserved "if"
    condition <- b_expression
    m_reserved "then"
    then_branch <- statement
    return (If condition then_branch)


def :: LanguageDef ()
def = emptyDef {
    opStart = oneOf "+-*/",
    opLetter = oneOf "",
    reservedOpNames = ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=", "and", "or", "not"],
    reservedNames  = ["true", "false", "and", "or", "not", "if", "then", "else", "do", "while"]
}

TokenParser {
    naturalOrFloat  = m_naturalOrFloat,
    parens = m_parens,
    reservedOp = m_reservedOp,
    reserved  = m_reserved,
    whiteSpace = m_whiteSpace,
    semi       = m_semi
} = makeTokenParser def

expression :: Parser Expr
expression = (BE <$> (try b_expression)) <|> (AE <$> (try a_expression)) <|> ternary

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
    <|> r_expression

r_expression :: Parser BExpr
r_expression = do
    a1 <- a_expression
    opp <- r_opp
    a2 <- a_expression
    return (RExpr a1 a2 opp)
        where
            r_opp :: Parser ROpperator
            r_opp = (m_reservedOp ">" >> return Greater)
                <|> (m_reservedOp "<" >> return Less)
                <|> (m_reservedOp ">=" >> return GreaterEqual)
                <|> (m_reservedOp "<=" >> return LessEqual)
                <|> (m_reservedOp "==" >> return DoubleEquals)
                <|> (m_reservedOp "!=" >> return NotEquals)


ternary :: Parser Expr
ternary = do
    m_reserved "if"
    condition <- b_expression
    m_reserved "then"
    then_branch <- expression
    m_reserved "else"
    else_branch <- expression
    return (Ternary condition then_branch else_branch)

my_parse :: String -> Either ParseError Stmt
my_parse source = parse (m_whiteSpace >> program) "" source