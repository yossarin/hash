module Hash.Parsing.HashParser where

import Hash.Language.Expressions
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, oneOf, noneOf, digit, char, letter, spaces, alphaNum, newline, anyChar)
import Text.Parsec (parse, ParseError, try, optionMaybe, option, choice, skipMany1, skipMany)
import Text.Parsec.Combinator (many1, sepBy1, between, manyTill)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), (<|>), Applicative, many)
import Data.Char (digitToInt)

err = "An error has occurred"

keywords = ["if", "fi", "then", "else"]

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse p err

-- Parsing of positive number
number :: Parser Int
number = read <$> many1 digit

-- Operator makes this kind of application a little nicer
infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

-- Parses a negative integer
negative :: Parser Int
negative = read <$> char '-' <:> many1 digit

-- Parses a negaive or a positive integer
integer :: Parser Int
integer = number <|> negative

-- Application of this parser disregrds trailing spaces
token :: Parser a -> Parser a
token = (<* spaces)

-- Application of this parser disregards prepending spaces
sp :: Parser a -> Parser a
sp = (spaces *>)

-- Parses a single character
symbol :: Char -> Parser Char
symbol = token . char

-- Comment parser
comment :: Parser String 
comment = do
  char '#'
  comment <- (manyTill anyChar newline)
  return ""

-- Function that parses the string using a given parser.
-- Disregardnig leading spaces.
betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

-- Parses both negative and positive integers, disregardes leading spaces
integer' :: Parser Int
integer' = token integer

-- Parsing a string of characters
stringOfChars :: Parser Expr
stringOfChars = Str <$> (token . many1 $ noneOf "\n\" $<>[]{}();")

-- Parsing the variable name, first character must be a letter,
-- the rest can be alphaNum
variable :: Parser Expr 
variable = Var <$> letter <:> many alphaNum

-- Parsing a reference to a variable (reference to a variable a is $a)
refToVar :: Parser Expr
refToVar = Var <$> (char '$' *> (token $ letter <:> many alphaNum))

-- Parses content between double quotes
stringLiteral :: Parser Expr
stringLiteral = Str <$> (char '"' *> many (noneOf "\"") <* char '"')

-- Parses either a variable name, string of characters or 
-- string snclosed in "double quotes"
expression :: Parser Expr
expression = try stringOfChars <|> stringLiteral <|> variable <|> refToVar

-- Tokenizes the expression
texpr = token expression

-- Parses the assignemt to a variabl
assignment :: Parser Cmd
assignment = Assign <$> (token $ variable) <*> (symbol '=' *> expression) <* symbol ';'

-- Parsing the input redirect
input :: Parser (Maybe Expr)
input = optionMaybe $ symbol '<' *> (token expression)

-- Parsing the output redirect
output :: Parser (Maybe Expr)
output = optionMaybe $ symbol '>' *> (token expression)

-- Parsing the output append
outputAppend :: Parser (Maybe Expr)
outputAppend = optionMaybe $ string ">>" *> spaces *> (token expression)

applyBool :: Bool -> (Bool -> Cmd) -> Cmd
applyBool b f = f b

-- Parse non appending command
commandNonAppending = (applyBool False) <$> (Cmd <$> texpr <*> (many texpr) <*> input <*> output <* symbol ';')

-- Parse appending commmand
commandAppending = (applyBool True) <$> (Cmd <$> texpr <*> (many texpr) <*> input <*> outputAppend <* symbol ';')

-- Cmd parser (if there is no output redirect it defaults to non-appending)
command :: Parser Cmd
command = try commandNonAppending <|> commandAppending

-- Parser for command or assigment
cmd :: Parser Cmd
cmd = try assignment <|> command

-- Parses a comparison operator (==, /=, <, >, >=, <=)
compOp :: Parser String 
compOp = token . many1 $ oneOf "=/<>"

-- Returns a corresponding comparrison operator
strToOperator :: Expr -> String -> Expr -> Comp
strToOperator e1 s e2 = case s of
  "==" -> CEQ e1 e2
  "/=" -> CNE e1 e2
  ">=" -> CGE e1 e2
  ">"  -> CGT e1 e2
  "<=" -> CLE e1 e2
  "<"  -> CLT e1 e2

-- Parsing comparison of expressions
comp :: Parser Comp
comp = strToOperator <$> expression <*> compOp <*> expression

-- Table of logical operators
table = [[unary '!' Not, binary '|' Or, binary '&' And]]
  where binary name f = Infix  (f <$ (symbol name)) AssocLeft
        unary  name f = Prefix (f <$ (symbol name))

-- Parser for logic expressions
predicates :: Parser Pred
predicates = buildExpressionParser table other
  where other       = rootCase <|> parenthases
        rootCase    = Pred <$> (token $ comp)
        parenthases = Parens <$> (char '(' *> spaces *> predicates <* char ')' <* spaces)

-- If prser
ifParser :: Parser Conditional
ifParser = If <$> (string "if" *> sp predicates) <*> (sp $ string "then" *> spaces *> symbol '{' *> many (sp cmd) <* spaces <* symbol '}')

-- If-Then-Else parser
ifThenElseParser :: Parser Conditional
ifThenElseParser = IfElse <$> (string "if" *> sp predicates) <*> (sp $ string "then" *> spaces *> symbol '{' *> many (sp cmd) <* spaces <* symbol '}' <* string "else" <* spaces <* symbol '{' ) <*> (many (sp cmd) <* spaces <* symbol '}')

-- Conditional branch parser
cbp :: Parser Conditional
cbp = try ifThenElseParser <|> ifParser

-- TLExpr parser, tries to parse a comment, conditional branch or a command
tle :: Parser TLExpr
tle = (skipMany (token comment)) *> (try (TLCnd <$> cbp) <|> (TLCmd <$> cmd)) <* (skipMany (token comment))
