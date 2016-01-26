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
    | LRational {dividend :: Integer, divisor :: Integer}
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
    show (LList xs) = "(" ++ ((intercalate ", ") . (map show)) xs ++ ")"
    show (LFailure a) = "LFailure" ++ a

  normalizeRational :: Integer -> Integer -> LispVal
  normalizeRational dividend divisor
    | normalDivisor == 1 = LInteger normalDividend
    | otherwise = LRational normalDividend normalDivisor
    where normalizer = gcd dividend divisor
          normalDividend = dividend `div` normalizer
          normalDivisor = divisor `div` normalizer

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
    let characteristic = read xs :: Integer
    (parseFloat characteristic) <|> (parseIntPower characteristic) <|> (return $ LInteger characteristic)

  parseExactPoweredFloat :: Integer -> Integer -> Parser LispVal
  parseExactPoweredFloat dividend divisor = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ normalizeRational (dividend * 10 ^ exponent) (divisor)

  parseExactFloat :: Integer -> Parser LispVal
  parseExactFloat characteristic = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read xs :: Integer
        exponent = length xs
        divisor = 10 ^ exponent
        dividend = ((characteristic * divisor) + mantissa)
    (parseExactPoweredFloat dividend divisor) <|> (return $ normalizeRational dividend divisor)

  parseExactNumber :: Parser LispVal
  parseExactNumber = do
    xs <- digitsDirectlyAfter 'e'
    let characteristic = read xs :: Integer
    (parseIntPower characteristic) <|> (parseExactFloat characteristic) <|> (return $ LInteger characteristic)

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
  listEval (func : head : tails) = foldl (numericBinop func) head tails

  numericBinop :: LispVal -> (LispVal -> LispVal -> LispVal)
  numericBinop (LAtom "+") = lAdd
  numericBinop (LAtom "-") = lSub
  numericBinop (LAtom "*") = lMul
  numericBinop (LAtom "/") = lDiv

  lDiv :: LispVal -> LispVal -> LispVal
  -- integer division
  lDiv i1@(LInteger _) i2@(LInteger _) = lDiv (intToRational i1) (intToRational i2)
  lDiv i@(LInteger _) (LRational dividend divisor) = lMul i (LRational divisor dividend)
  lDiv (LInteger i) (LFloat f) = LFloat $ (fromIntegral i) / f
  -- rational division
  lDiv r@(LRational _ _) i@(LInteger _) = lDiv r (intToRational i)
  lDiv r@(LRational _  _) (LRational dividend divisor) = lMul r (LRational divisor dividend)
  lDiv (LRational dividend divisor) (LFloat f) = LFloat $ finalDividend / finalDivisor
    where floatDividend = fromIntegral dividend
          floatDivisor = fromIntegral divisor
          finalDividend = floatDividend / floatDivisor
          finalDivisor = f
  -- float division
  lDiv (LFloat f) (LInteger i) = LFloat (f / (fromIntegral i))
  lDiv f@(LFloat _) (LRational dividend divisor) = lMul f (LRational divisor dividend)
  lDiv (LFloat f1) (LFloat f2) = LFloat (f1 / f2)

  intToRational :: LispVal -> LispVal
  intToRational (LInteger i) = LRational i 1

  lAdd :: LispVal -> LispVal -> LispVal
  lAdd (LInteger i1) (LInteger i2) = LInteger (i1 + i2)
  lAdd (LInteger i) r@(LRational _ _) = lRationalAdd (LRational i 1) r
  lAdd (LInteger i) (LFloat f) = (LFloat $ (fromIntegral i) + f)
  lAdd r1@(LRational _ _) r2@(LRational _ _) = lRationalAdd r1 r2
  lAdd r@(LRational _ _) i@(LInteger _) = lAdd i r
  lAdd (LRational dividend divisor) (LFloat f) = LFloat $ fr + f
    where
      a = fromIntegral dividend
      b = fromIntegral divisor
      fr = a / b
  lAdd f@(LFloat _) i@(LInteger _) = lAdd i f
  lAdd f@(LFloat _) r@(LRational _ _) = lAdd r f
  lAdd (LFloat f1) (LFloat f2) = LFloat (f1 + f2)

  lRationalAdd :: LispVal -> LispVal -> LispVal
  lRationalAdd (LRational dividend1 divisor1) (LRational dividend2 divisor2) = normalizeRational dividendResult divisorResult
    where normal1 = LRational (dividend1 * divisor2) (divisor1 * divisor2)
          normal2 = LRational (dividend2 * divisor1) (divisor1 * divisor2)
          dividendResult = (dividend normal1) + (dividend normal2)
          divisorResult = (divisor1 * divisor2)

  lMul :: LispVal -> LispVal -> LispVal
  lMul (LInteger i1) (LInteger i2) = LInteger (i1 * i2)
  lMul (LInteger i) (LRational dividend divisor) = normalizeRational (i * dividend) divisor
  lMul (LInteger i) (LFloat f) = LFloat $ (fromIntegral i) * f
  lMul r@(LRational _ _) i@(LInteger _) = lMul i r
  lMul (LRational dividend1 divisor1) (LRational dividend2 divisor2) = normalizeRational (dividend1 * dividend2) (divisor1 * divisor2)
  lMul (LRational dividend divisor) (LFloat f) = (LFloat $ fr * f)
    where
      fr = a / b
      a = fromIntegral dividend
      b = fromIntegral divisor
  lMul f@(LFloat _) i@(LInteger _) = lMul i f
  lMul f@(LFloat _) r@(LRational _ _) = lMul r f
  lMul (LFloat f1) (LFloat f2) = LFloat $ f1 * f2

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
