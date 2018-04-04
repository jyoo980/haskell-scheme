module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad

-- Defining data our parser will return our parsed values as
data LispVal = Atom String
    | List[LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

-- Define a parser which recognizes the symbols shown below
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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

-- Parser which recognizes either:
--      String, Atom, Expression
parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber

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