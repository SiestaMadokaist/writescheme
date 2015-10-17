import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | String String
    | Bool Bool
    | Integer Integer
    | Exponent Integer
    | Float Float
    | Rational Integer Integer

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> symbol <|> digit)
        let atom = first:rest
        return $ Atom atom

parseTrue :: Parser LispVal
parseTrue = do
        char 't'
        return $ Atom "True"

parseFalse :: Parser LispVal
parseFalse = do
        char 'f'
        return $ Atom "False"

parseExponentiable :: (Num a) => a -> Parser LispVal
parseExponentiable = do
        char 'e'
        xs <- many digit
        let exponent = read xs :: (Num a)

-- s42
parseInexactNumber :: Parser LispVal
parseInexactNumber = do
        char 'i'
        xs <- many digit
        let beforeComma = read xs :: Integer
        (parseExponentiable beforeComma) <|> (parseFloatingPoint beforeComma)

-- s4
parseConstant :: Parser LispVal
parseConstant = do
        char '#'
        parseTrue <|> parseFalse <|> parseInexactNumber <|> parseDecimal <|> parseExactNumber <|> parseCharacter

escapedChars :: Parser Char
escapedChars = do
        char '\\'
        x <- oneOf "\\\"nrt"
        return x

parseString :: Parser LispVal
parseString = do
        char '"'
        xs <- many $ escapedChars <|> noneOf "\"\\"
        char '"'
        return $ String xs

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_-"

errPrint :: (Show a) => Either a b -> String
errPrint (Left err) = "No Match: " ++ show err
errPrint (Right val) = "Found Value"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = errPrint $ parse symbol "lisp" input

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
