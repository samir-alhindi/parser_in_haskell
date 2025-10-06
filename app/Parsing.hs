module Parsing where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity

import AST

program :: Parser [Stmt]
program = statement_sequence <* eof <?> "program"

statement_sequence :: Parser [Stmt]
statement_sequence = many1 statement
    
statement :: Parser Stmt
statement = try print' <|> try if_else <|> try if' <|> try let_binding <|> block <?> "statement"

block :: Parser Stmt
block = Block <$> (m_braces statement_sequence)

let_binding :: Parser Stmt
let_binding = do
    m_reserved "let"
    name <- m_identifier
    m_reservedOp "="
    initialization <- expression
    m_semi
    return (LetBinding name initialization)


print' :: Parser Stmt
print' = do
    m_reserved "print"
    expre <- expression
    m_semi
    return (Print expre) <?> "print"

if_else :: Parser Stmt
if_else = do
    m_reserved "if"
    condition <- expression
    m_reserved "then"
    then_branch <- statement
    m_reserved "else"
    else_branch <- statement
    return (IfElse condition then_branch else_branch)

if' :: Parser Stmt
if' = do
    m_reserved "if"
    condition <- expression
    m_reserved "then"
    then_branch <- statement
    return (If condition then_branch)


def :: LanguageDef ()
def = emptyDef {
    opStart = oneOf "+-*/><=!",
    opLetter = oneOf "<>=",
    reservedOpNames = ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=", "and", "or", "not", "=", "><"],
    reservedNames  = ["true", "false", "and", "or", "not", "if", "then", "else", "let", "in", "\\", "->"]
}

TokenParser {
    naturalOrFloat  = m_naturalOrFloat,
    parens = m_parens,
    braces = m_braces,
    reservedOp = m_reservedOp,
    reserved  = m_reserved,
    identifier = m_identifier,
    whiteSpace = m_whiteSpace,
    semi       = m_semi,
    stringLiteral  = m_stringLiteral
} = makeTokenParser def

expression :: Parser Expr
expression = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term = try call <|> atom

atom :: Parser Expr
atom = m_parens expression
    <|> try number
    <|> try ternary
    <|> try (StringExpr <$> m_stringLiteral)
    <|> try boolean
    <|> try identifier'
    <|> try lambda
    <|> try let_expr

table :: [[Operator String () Identity Expr]]
table = [
    [Prefix (m_reservedOp "-" >> return ((Unary Negation)))           ],

    [Infix  (m_reservedOp "*" >> return (Binary Multiply)) AssocLeft,
     Infix  (m_reservedOp "/" >> return (Binary Divide))   AssocLeft],

    [Infix  (m_reservedOp "+" >> return (Binary Plus))     AssocLeft,
     Infix  (m_reservedOp "-" >> return (Binary Minus))    AssocLeft],

    [Infix (m_reservedOp ">" >> return (Binary Greater)) AssocNone,
     Infix (m_reservedOp "<" >> return (Binary Less))    AssocNone,
     Infix (m_reservedOp ">=" >> return (Binary GreaterEqual))   AssocNone,
     Infix (m_reservedOp "<=" >> return (Binary LessEqual))   AssocNone],

    [Infix (m_reservedOp "==" >> return (Binary DoubleEquals)) AssocLeft,
     Infix (m_reservedOp "!=" >> return (Binary NotEquals))   AssocLeft],

    [Prefix (m_reservedOp "not" >> return (Unary Not))                              ],
    [Infix  (m_reservedOp "and" >> return (Binary And)) AssocLeft                    ],
    [Infix  (m_reservedOp "or" >> return (Binary Or)) AssocLeft                      ],
    
    [Infix (m_reservedOp "><" >> return (Binary Bind)) AssocLeft]]    

number :: Parser Expr
number = f <$> m_naturalOrFloat
    where
        f :: Either Integer Double -> Expr
        f (Left i) = Number (fromIntegral i)
        f (Right d)  = Number d

identifier' :: Parser Expr
identifier' = Name <$> m_identifier

boolean :: Parser Expr
boolean = (m_reserved "true"   >> return (Boolean True ))
    <|> (m_reserved "false" >> return (Boolean False))

let_expr :: Parser Expr
let_expr = do
    m_reserved "let"
    name <- m_identifier
    m_reservedOp "="
    init <- expression
    m_reserved "in"
    result <- expression
    return (LetExpr name init result)


lambda :: Parser Expr
lambda = do
    m_reserved "\\"
    parameters <- many1 m_identifier
    m_reserved "->"
    body <- expression
    return (Lambda parameters body)

call :: Parser Expr
call = do
    callee <- valid_callee <|> (m_parens valid_callee)
    args <- many1 expression
    return (Call callee args)
        where
            valid_callee :: Parser Expr
            valid_callee = identifier' <|> lambda <|> (m_parens expression)

ternary :: Parser Expr
ternary = do
    m_reserved "if"
    condition <- expression
    m_reserved "then"
    then_branch <- expression
    m_reserved "else"
    else_branch <- expression
    return (Ternary condition then_branch else_branch)

my_parse :: String -> Either ParseError [Stmt]
my_parse source = parse (m_whiteSpace >> program) "" source