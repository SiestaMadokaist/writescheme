{-# LANGUAGE QuasiQuotes #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.String.Interpolate
import Data.List
import System.Environment
import Control.Monad as M

data LispVal = Atom String
             | String String
             | List [LispVal]
             deriving (Show, Eq)

showVal :: LispVal -> String
showVal (String s) = "String " ++ s

parseString :: Parser LispVal
parseString = do
    char '"'
    xs <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String xs

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\"\\nrt"
    return x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    return $ Atom $ first:rest

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_-"

spaces :: Parser String
spaces = many1 $ oneOf " "

parseListExpr :: Parser LispVal
parseListExpr = do
    char '('
    items <- sepBy parseExpr spaces
    char ')'
    return $ List $ items

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseListExpr

errPrint :: (Show a) => Either a LispVal -> String
errPrint (Left err) = [i|No Match: #{err}|]
errPrint (Right val) = [i|Found Value: #{val}|]

readExpr :: String -> String
readExpr input = errPrint $ parse parseExpr "lisp" input
