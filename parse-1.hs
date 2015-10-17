import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispNumber = Integer Integer
                | Float Float
                | Rational {dividend :: Integer, divisor :: Integer}
                deriving (Show)

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | String String
    | Bool Bool
    | LispNumber LispNumber
    deriving (Show)

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> symbol <|> digit)
        let atom = first:rest
        return $ Atom atom

parseTrue :: Parser LispVal
parseTrue = do
        char 't'
        return $ Bool True

parseFalse :: Parser LispVal
parseFalse = do
        char 'f'
        return $ Bool False

parseExponentiable :: LispNumber -> Parser LispVal
parseExponentiable (Integer base) = do
        char 'e'
        xs <- many digit
        let exponent = read xs :: Integer
        return $ LispNumber $ Integer $ base * (10 ^ exponent)
parseExponentiable (Float base) = do
        char 'e'
        xs <- many digit
        let exponent = read xs :: Float
        return $ LispNumber $ Float $ base * 10 ** exponent
parseExponentiable (Rational dividend divisor)= do
        char 'e'
        xs <- many digit
        let exponent = read xs :: Integer
            dividend2 = dividend * (10 ^ exponent)
        return $ LispNumber $ Rational dividend2 divisor

parseFloatingPoint :: LispNumber -> Parser LispVal
parseFloatingPoint (Float f) = do
       char '.'
       xs <- many digit
       let decimalPoint = read ("0." ++ xs) :: Float
           floatValue = f + decimalPoint
           parseFloat = do
                return $ LispNumber $ Float floatValue
       (parseExponentiable $ Float floatValue) <|> parseFloat
parseFloatingPoint (Integer i) = do
        char '.'
        xs <- many digit
        let decimalPoint = read xs :: Integer
            divisor = 10 ^ length xs
            iBig = i * divisor
            dividend = iBig + decimalPoint
            rationalValue = Rational dividend divisor
            parseInt = do
                return $ LispNumber $ rationalValue
        (parseExponentiable $ rationalValue) <|> parseInt

-- s42
parseInexactNumber :: Parser LispVal
parseInexactNumber = do
        char 'i'
        xs <- many digit
        let bigNum = read xs :: Float
            fbigNum = Float bigNum
        (parseExponentiable fbigNum) <|> (parseFloatingPoint fbigNum)

parseExactNumber ::  Parser LispVal
parseExactNumber = do
        char 'e'
        xs <- many digit
        let bigNum = read xs :: Integer
            ibigNum = Integer bigNum
        (parseExponentiable ibigNum) <|> (parseFloatingPoint ibigNum)

parseDecimal :: Parser LispVal
parseDecimal = do
        char 'd'
        xs <- many digit
        let ibigNum = read xs :: Integer
            fbigNum = read xs :: Float
        (parseExponentiable $ Integer ibigNum) <|> (parseFloatingPoint $ Float fbigNum)

parseDigit :: Parser LispVal
parseDigit = do
        xs <- many digit
        let ibigNum = read xs :: Integer
            fbigNum = read xs :: Float
            parseBasicDigit = do
                return $ LispNumber $ Integer ibigNum
        (parseExponentiable $ Integer ibigNum) <|> (parseFloatingPoint $ Float fbigNum) <|> parseBasicDigit

-- s4
parseSymbol :: Parser LispVal
parseSymbol = do
        char '#'
        parseTrue <|> parseFalse <|> parseInexactNumber <|>  parseExactNumber <|> parseDecimal -- <|> parseChar

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

errPrint :: (Show a, Show b) => Either a b -> String
errPrint (Left err) = "No Match: " ++ show err
errPrint (Right val) = "Found Value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseDigit <|> parseAtom <|> parseSymbol

readExpr :: String -> String
readExpr input = errPrint $ parse parseExpr "lisp" input

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
