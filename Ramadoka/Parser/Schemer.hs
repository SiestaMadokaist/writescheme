{-# LANGUAGE QuasiQuotes #-}
module Ramadoka.Parser.Schemer
( showVal,
  readExpr,
  getExpr,
  LispNumber(..),
  LispVal(..)
) where
    import Text.ParserCombinators.Parsec hiding (spaces)
    import Data.List
    import System.Environment
    import Control.Monad as M
    import Data.String.Interpolate

    data LispNumber = Integer Integer
                    | Float Float
                    | Rational {dividend :: Integer, divisor :: Integer}
                    deriving (Show, Eq)

    data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | String String
        | Char Char
        | Bool Bool
        | LispNumber LispNumber
        deriving (Eq)

    showVal :: LispVal -> String
    showVal (String s) = "String " ++ s
    showVal (Char c) = [c]
    showVal (Bool b) = show b
    showVal (LispNumber x) = show x
    showVal (List xs) = "(" ++ unwordsList xs ++ ")"
    showVal (DottedList head tail) = [i|(#{unwordHead} . #{showValTail})|]
      where unwordHead = unwordsList head
            showValTail = showVal tail
    showVal (Atom a) = "Atom " ++ a

    instance Show LispVal where
      show = showVal

    unwordsList :: [LispVal] -> String
    unwordsList = (intercalate ", ") . (map showVal)

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
            xs <- many1 digit
            let exponent = read xs :: Integer
            return $ LispNumber $ Integer $ base * (10 ^ exponent)
    parseExponentiable (Float base) = do
            char 'e'
            xs <- many1 digit
            let exponent = read xs :: Float
            return $ LispNumber $ Float $ base * 10 ** exponent
    parseExponentiable (Rational dividend divisor) = do
            char 'e'
            xs <- many1 digit
            let exponent = read xs :: Integer
                dividend2 = dividend * (10 ^ exponent)
            return $ LispNumber $ Rational dividend2 divisor

    parseFloatingPoint :: LispNumber -> Parser LispVal
    parseFloatingPoint (Float f) = do
           char '.'
           xs <- many1 digit
           let decimalPoint = read ("0." ++ xs) :: Float
               floatValue = f + decimalPoint
               parseFloat = return $ LispNumber $ Float floatValue
           (parseExponentiable $ Float floatValue) <|> parseFloat
    parseFloatingPoint (Integer i) = do
            char '.'
            xs <- many1 digit
            let decimalPoint = read xs :: Integer
                divisor = 10 ^ length xs
                iBig = i * divisor
                dividend = iBig + decimalPoint
                rationalValue = Rational dividend divisor
                parseInt = return $ LispNumber $ rationalValue
            (parseExponentiable rationalValue) <|> parseInt

    -- s42
    parseInexactNumber :: Parser LispVal
    parseInexactNumber = do
            char 'i'
            xs <- many1 digit
            let bigNum = read xs :: Float
                fbigNum = Float bigNum
            (parseExponentiable fbigNum) <|> (parseFloatingPoint fbigNum)

    parseExactNumber ::  Parser LispVal
    parseExactNumber = do
            char 'e'
            xs <- many1 digit
            let bigNum = read xs :: Integer
                ibigNum = Integer bigNum
                parseInt = return $ LispNumber ibigNum
            (parseExponentiable ibigNum) <|> (parseFloatingPoint ibigNum) <|> parseInt

    parseDecimal :: Parser LispVal
    parseDecimal = do
            char 'd'
            xs <- many1 digit
            let ibigNum = read xs :: Integer
                fbigNum = read xs :: Float
            (parseExponentiable $ Integer ibigNum) <|> (parseFloatingPoint $ Float fbigNum)

    parseDigit :: Parser LispVal
    parseDigit = do
            xs <- many1 digit
            let ibigNum = read xs :: Integer
                fbigNum = read xs :: Float
                parseBasicDigit = return $ LispNumber $ Integer ibigNum
            (parseExponentiable $ Integer ibigNum) <|> (parseFloatingPoint $ Float fbigNum) <|> parseBasicDigit

    parseChar :: Parser LispVal
    parseChar = do
            char '\\'
            ch <- letter
            return $ Char $  ch
    -- s4
    parseSymbol :: Parser LispVal
    parseSymbol = do
            char '#'
            parseTrue <|> parseFalse <|> parseChar <|> parseInexactNumber <|>  parseExactNumber <|> parseDecimal

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

    errPrint :: (Show a) => Either a LispVal -> String
    errPrint (Left err) = "No Match: " ++ show err
    errPrint (Right val) = "Found Value: " ++ showVal val

    spaces :: Parser ()
    spaces = skipMany1 space

    spaceOrDots :: Parser ()
    spaceOrDots = do
            skipMany spaces
            char '.'
            skipMany spaces

    parseList :: Parser LispVal
    parseList = do
            char '('
            exprs <- sepBy parseExpr spaces
            char ')'
            return $ List exprs

    parseDottedList :: Parser LispVal
    parseDottedList = do
            char '('
            exprs <- sepBy parseExpr spaceOrDots
            char ')'
            let lst = last exprs
                ints = init exprs
            return $ DottedList ints lst

    parseDotOrNormalList :: Parser LispVal
    parseDotOrNormalList = do
            (try parseList) <|> (try parseDottedList)

    parseQuoted :: Parser LispVal
    parseQuoted = do
            char '`'
            exprs <- parseExpr
            return $ List [Atom "quote", exprs]

    parseExpr :: Parser LispVal
    parseExpr = parseString <|> parseDigit <|> parseAtom <|> parseSymbol <|> parseDotOrNormalList <|> parseQuoted

    getExpr :: String -> Either ParseError LispVal
    getExpr = parse parseExpr "lisp"

    eval :: LispVal -> LispVal
    eval val@(String _) = val
    eval val@(LispNumber _) = val
    eval val@(Bool _) = val
    eval (List [Atom "quote", val]) = val

    takeRight :: Either ParseError LispVal -> LispVal
    takeRight (Right val) = val

    readExpr :: String -> String
    readExpr input = errPrint $ getExpr input

    main :: IO()
    main = getArgs >>= print . eval . takeRight . getExpr . head
