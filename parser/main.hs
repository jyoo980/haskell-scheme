module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Defining data our parser will return our parsed values as
data LispVal = Atom String
    | List[LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Fractional Double
    | String String
    | Bool Bool

-- Define a parser which recognizes the symbols shown below
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Parsing Lists
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parsing dotted List notation
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- Support for Lisp single-quotes
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List[Atom "quote", x]

-- Parsing strings
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

-- Parsing scheme variables
-- ATOM: a letter/symbol, followed by any number of letter/symbol(s)
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- Parsing Numbers
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Parsing doubles (decimal numbers)
parseDouble :: Parser LispVal
parseDouble = liftM (Fractional . read) $ many1 digit

-- Parser which recognizes either:
--      String, Atom, Expression
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
            
-- Parser which recongizes spaces
spaces :: Parser()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn(readExpr expr)