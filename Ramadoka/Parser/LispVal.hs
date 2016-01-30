{-# LANGUAGE QuasiQuotes #-}
module Ramadoka.Parser.LispVal
(
  readExpr,
  getExpr,
  normalizeRational,
  LispVal(..),
  eval,
) where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Data.List
  import System.Environment
  import Control.Monad as M
  import Data.String.Interpolate

  data LispVal = LInteger Integer
    | LFloat Float
    | LRational {numerator :: Integer, denominator :: Integer}
    | LBool Bool
    | LAtom String
    | LChar Char
    | LString String
    | LList [LispVal]
    | LDottedList [LispVal]
    | LFailure String
    deriving (Eq)

  instance Show LispVal where
    show (LInteger a) = show a
    show (LFloat a) = show a
    show (LRational a b) = (show a) ++ "/" ++ (show b)
    show (LChar a) = show a
    show (LAtom a) = a
    show (LString a) = a
    show (LList xs) = stringify xs
    show (LFailure a) = "LFailure" ++ a

  stringify :: [LispVal] -> String
  stringify xs = ((intercalate ", ") . (map show)) xs

  normalizeRational :: Integer -> Integer -> LispVal
  normalizeRational numerator denominator
    | normalDivisor == 1 = LInteger normalDividend
    | otherwise = LRational normalDividend normalDivisor
    where normalizer = gcd numerator denominator
          normalDividend = numerator `div` normalizer
          normalDivisor = denominator `div` normalizer

  errPrint :: (Show a) => Either a LispVal -> String
  errPrint (Left err) = "No Match: " ++ show err
  errPrint (Right val) = "Found Value: " ++ show val

  escapeCharacters :: Parser Char
  escapeCharacters = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return x

  symbol :: Parser Char
  symbol = oneOf "!$%&|*+-/:<=>?@^_-"

  digitsDirectlyAfter :: Char -> Parser String
  digitsDirectlyAfter c = do
    char c
    xs <- many1 digit
    return xs

  spaces :: Parser ()
  spaces = skipMany1 space

  spaceOrDots :: Parser ()
  spaceOrDots = do
          skipMany spaces
          char '.'
          skipMany spaces

  -- Parser LispVal --

  parseFloat :: Integer -> Parser LispVal
  parseFloat i = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read ("0." ++ xs)
        currentResult = return $ LFloat (fromIntegral i + mantissa)
    (parseFloatPower $ fromIntegral i + mantissa) <|> currentResult

  parseIntPower :: Integer -> Parser LispVal
  parseIntPower base = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ LInteger $ base * (10 ^ exponent)

  parseFloatPower :: Float -> Parser LispVal
  parseFloatPower base = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Float
    return $ LFloat $ base * (10 ** exponent)

  parseNumber :: Parser LispVal
  parseNumber = do
    xs <- many1 digit
    let currentNumber = read xs :: Integer
        -- our current number is probably a:
        -- base for a power
        -- characteristic of
        base = currentNumber
        numerator = currentNumber
        characteristic = currentNumber
    (parseRational numerator) <|> (parseFloat characteristic) <|> (parseIntPower base) <|> (return $ LInteger characteristic)

  parseExactPoweredFloat :: Integer -> Integer -> Parser LispVal
  parseExactPoweredFloat numerator denominator = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ normalizeRational (numerator * 10 ^ exponent) (denominator)

  parseExactFloat :: Integer -> Parser LispVal
  parseExactFloat characteristic = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read xs :: Integer
        exponent = length xs
        denominator = 10 ^ exponent
        numerator = ((characteristic * denominator) + mantissa)
    (parseExactPoweredFloat numerator denominator) <|> (return $ normalizeRational numerator denominator)

  parseRational :: Integer -> Parser LispVal
  parseRational numerator = do
    xs <- digitsDirectlyAfter '/'
    let denominator = read xs :: Integer
    return $ normalizeRational numerator denominator

  parseExactNumber :: Parser LispVal
  parseExactNumber = do
    xs <- digitsDirectlyAfter 'e'
    let characteristic = read xs :: Integer
        numerator = characteristic
    (parseRational numerator) <|> (parseIntPower characteristic) <|> (parseExactFloat characteristic) <|> (return $ LInteger characteristic)

  parseInexactNumber :: Parser LispVal
  parseInexactNumber = do
    xs <- digitsDirectlyAfter 'i'
    let characteristic = read xs :: Float
        directResult = return $ LFloat characteristic
    (parseFloatPower characteristic) <|> (parseFloat $ floor characteristic) <|> (directResult)

  parseTrue :: Parser LispVal
  parseTrue = do
    char 't'
    return $ LBool True

  parseFalse :: Parser LispVal
  parseFalse = do
    char 'f'
    return $ LBool False

  parseAtom :: Parser LispVal
  parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
        in return $ LAtom atom

  parseSymbolic :: Parser LispVal
  parseSymbolic = do
    char '#'
    parseTrue <|> parseFalse <|> parseExactNumber <|> parseInexactNumber

  parseString :: Parser LispVal
  parseString = do
    char '"'
    xs <- many (escapeCharacters <|> noneOf "\"\\")
    char '"'
    return $ LString xs

  parseList :: Parser LispVal
  parseList = do
    char '('
    exprs <- sepBy parseExpr spaces
    char ')'
    return $ LList exprs

  parseDottedList :: Parser LispVal
  parseDottedList = do
    char '('
    exprs <- sepBy parseExpr spaceOrDots
    char ')'
    return $ LDottedList exprs

  parseQuoted :: Parser LispVal
  parseQuoted = do
    char '`'
    expr <- parseExpr
    return $ LList [LAtom "quote", expr]

  parseExpr :: Parser LispVal
  parseExpr = parseSymbolic <|> (parseFloat 0) <|> parseNumber <|> parseString <|> parseAtom <|> parseQuoted <|> (try parseList) <|> (try parseDottedList)

  -- end of parser --

  eval :: LispVal -> LispVal
  eval val@(LBool _) = val
  eval val@(LInteger _) = val
  eval val@(LFloat _) = val
  eval val@(LRational _ _) = val
  eval val@(LString _) = val
  eval (LList [LAtom "quote", expr]) = expr
  eval (LList exprs) = listEval exprs

  listEval :: [LispVal] -> LispVal
  listEval (func : head : []) = (unOp func) head
  listEval (func : head : tails) = foldl (binOp func) head tails

  isSymbol :: LispVal -> LispVal
  isSymbol (LAtom _) = LBool True
  isSymbol (LList exprs) = isSymbol $ listEval exprs
  isSymbol _ = LBool False

  isString :: LispVal -> LispVal
  isString (LString _) = LBool True
  isString (LList exprs) = isString $ listEval exprs
  isString _ = LBool False

  isNumber :: LispVal -> LispVal
  isNumber (LRational _ _) = LBool True
  isNumber (LInteger _) = LBool True
  isNumber (LFloat _)  = LBool True
  isNumber (LList exprs) = isNumber $ listEval exprs
  isNumber _ = LBool False

  unOp :: LispVal -> (LispVal -> LispVal)
  unOp (LAtom "symbol?") = isSymbol
  unOp (LAtom "string?") = isString
  unOp (LAtom "number?") = isNumber

  binOp :: LispVal -> (LispVal -> LispVal -> LispVal)
  binOp (LAtom "+") = lAdd
  binOp (LAtom "-") = lSub
  binOp (LAtom "*") = lMul
  binOp (LAtom "/") = lDiv

  lDiv :: LispVal -> LispVal -> LispVal
  -- integer division
  lDiv (LInteger numerator) (LInteger denominator) = normalizeRational numerator denominator
  lDiv (LInteger i) (LFloat f) = LFloat $ (fromIntegral i) / f
  -- rational division
  lDiv r@(LRational _ _) i@(LInteger _) = lDiv r (intToRational i)
  lDiv (LRational numerator denominator) (LFloat f) = LFloat $ finalDividend / finalDivisor
    where floatDividend = fromIntegral numerator
          floatDivisor = fromIntegral denominator
          finalDividend = floatDividend / floatDivisor
          finalDivisor = f
  -- float division
  lDiv (LFloat f) (LInteger i) = LFloat (f / (fromIntegral i))
  lDiv (LFloat f1) (LFloat f2) = LFloat (f1 / f2)
  lDiv n (LRational numerator denominator) = lMul n (LRational denominator numerator)
  lDiv var1@(LList _) var2@(LList _) = lDiv (eval var1) (eval var2)
  lDiv var1@(LList _) var2 = lDiv (eval var1) var2
  lDiv var1 var2@(LList _) = lDiv var1 (eval var2)

  intToRational :: LispVal -> LispVal
  intToRational (LInteger i) = LRational i 1

  lAdd :: LispVal -> LispVal -> LispVal
  lAdd (LInteger i1) (LInteger i2) = LInteger (i1 + i2)
  lAdd (LInteger i) r@(LRational _ _) = lRationalAdd (LRational i 1) r
  lAdd (LInteger i) (LFloat f) = (LFloat $ (fromIntegral i) + f)
  lAdd r1@(LRational _ _) r2@(LRational _ _) = lRationalAdd r1 r2
  lAdd r@(LRational _ _) i@(LInteger _) = lAdd i r
  lAdd (LRational numerator denominator) (LFloat f) = LFloat $ fr + f
    where
      a = fromIntegral numerator
      b = fromIntegral denominator
      fr = a / b
  lAdd f@(LFloat _) i@(LInteger _) = lAdd i f
  lAdd f@(LFloat _) r@(LRational _ _) = lAdd r f
  lAdd (LFloat f1) (LFloat f2) = LFloat (f1 + f2)
  lAdd var1@(LList _) var2@(LList _) = lAdd (eval var1) (eval var2)
  lAdd var1@(LList _) var2 = lAdd (eval var1) var2
  lAdd var1 var2@(LList _) = lAdd var1 (eval var2)

  lRationalAdd :: LispVal -> LispVal -> LispVal
  lRationalAdd (LRational numerator1 denominator1) (LRational numerator2 denominator2) = normalizeRational numeratorResult denominatorResult
    where normal1 = LRational (numerator1 * denominator2) (denominator1 * denominator2)
          normal2 = LRational (numerator2 * denominator1) (denominator1 * denominator2)
          numeratorResult = (numerator normal1) + (numerator normal2)
          denominatorResult = (denominator1 * denominator2)

  lMul :: LispVal -> LispVal -> LispVal
  lMul (LInteger i1) (LInteger i2) = LInteger (i1 * i2)
  lMul (LInteger i) (LRational numerator denominator) = normalizeRational (i * numerator) denominator
  lMul (LInteger i) (LFloat f) = LFloat $ (fromIntegral i) * f
  lMul r@(LRational _ _) i@(LInteger _) = lMul i r
  lMul (LRational numerator1 denominator1) (LRational numerator2 denominator2) = normalizeRational (numerator1 * numerator2) (denominator1 * denominator2)
  lMul (LRational numerator denominator) (LFloat f) = (LFloat $ fr * f)
    where
      fr = a / b
      a = fromIntegral numerator
      b = fromIntegral denominator
  lMul f@(LFloat _) i@(LInteger _) = lMul i f
  lMul f@(LFloat _) r@(LRational _ _) = lMul r f
  lMul (LFloat f1) (LFloat f2) = LFloat $ f1 * f2
  lMul var1@(LList _) var2@(LList _) = lMul (eval var1) (eval var2)
  lMul var1@(LList _) var2 = lMul (eval var1) var2
  lMul var1 var2@(LList _) = lMul var1 (eval var2)

  lSub :: LispVal -> LispVal -> LispVal
  lSub a b = lAdd a $ lMul b (LInteger (-1))

  getExpr :: String -> Either ParseError LispVal
  getExpr = parse parseExpr "lisp"

  readExpr :: String -> String
  readExpr input = errPrint $ getExpr input

  main :: IO()
  main = do
    (expr:_) <- getArgs
    print $ readExpr $ expr
