{-# LANGUAGE QuasiQuotes #-}
module Ramadoka.Parser.LispVal
(
  readExpr,
  getExpr,
  normalizeRational,
  LispVal(..),
) where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Data.List
  import System.Environment
  import Control.Monad as M
  import Data.String.Interpolate

  data LispVal = LInteger Integer
    | LFloat Float
    | LRational {dividend :: Integer, divisor :: Integer}
    | LAtom String
    | LChar Char
    deriving (Eq)

  instance Show LispVal where
    show (LInteger a) = show a
    show (LFloat a) = show a
    show (LRational a b) = (show a) ++ "/" ++ (show b)
    show (LChar a) = show a
    show (LAtom a) = a

  normalizeRational :: Integer -> Integer -> LispVal
  normalizeRational a b = LRational (a `div` divisor) (b `div` divisor)
    where gcd a 0 = a
          gcd a b = gcd b (a `mod` b)
          divisor = gcd a b

  symbol :: Parser Char
  symbol = oneOf "!$%&|*+-/:<=>?@^_-"

  errPrint :: (Show a) => Either a LispVal -> String
  errPrint (Left err) = "No Match: " ++ err
  errPrint (Right val) = "Found Value: " ++ show val

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
    char '.'
    xs <- many1 digit
    let mantissa = read ("0." ++ xs)
        currentResult = return $ LFloat (fromIntegral i + mantissa)
    (parseFloatPower $ fromIntegral i + mantissa) <|> currentResult

  parseIntPower :: Integer -> Parser LispVal
  parseIntPower base = do
    char 'e'
    xs <- many1 digit
    let exponent = read xs :: Integer
    return $ LInteger $ base * (10 ^ exponent)

  parseFloatPower :: Float -> Parser LispVal
  parseFloatPower base = do
    char 'e'
    xs <- many1 digit
    let exponent = read xs :: Float
    return $ LFloat $ base * (10 ** exponent)

  parseInteger :: Integer -> Parser LispVal
  parseInteger i = return $ LInteger i

  parseNumber :: Parser LispVal
  parseNumber = do
    xs <- many1 digit
    let characteristic = read xs :: Integer
    (parseFloat characteristic) <|> (parseIntPower characteristic) <|> (parseInteger characteristic)

  parseExactPoweredFloat :: Integer -> Integer -> Parser LispVal
  parseExactPoweredFloat dividend divisor = do
    char 'e'
    xs <- many1 digit
    let exponent = read xs :: Integer
    return $ normalizeRational (dividend * 10 ^ exponent) (divisor)

  parseExactFloat :: Integer -> Parser LispVal
  parseExactFloat characteristic = do
    char '.'
    xs <- many1 digit
    let mantissa = read xs :: Integer
        exponent = length xs
        divisor = 10 ^ exponent
        dividend = ((characteristic * divisor) + mantissa)
    (parseExactPoweredFloat dividend divisor) <|> (return $ normalizeRational dividend divisor)

  parseExactNumber :: Parser LispVal
  parseExactNumber = do
    char 'e'
    xs <- many1 digit
    let characteristic = read xs :: Integer
    (parseIntPower characteristic) <|> (parseExactFloat characteristic) <|> (parseInteger characteristic)

  parseInexactNumber :: Parser LispVal
  parseInexactNumber = do
    char 'i'
    xs <- many1 digit
    let characteristic = read xs :: Float
        directResult = return $ LFloat characteristic
    (parseFloatPower characteristic) <|> (parseFloat $ floor characteristic) <|> (directResult)

  parseTrue :: Parser LispVal
  parseTrue = do
    char 't'
    return $ LAtom $ "True"

  parseFalse :: Parser LispVal
  parseFalse = do
    char 'f'
    return $ LAtom $ "False"

  parseSymbolic :: Parser LispVal
  parseSymbolic = do
    char '#'
    parseTrue <|> parseFalse <|> parseExactNumber <|> parseInexactNumber
    -- <|> parseCharacter

  parseExpr :: Parser LispVal
  parseExpr = parseSymbolic <|> (parseFloat 0) <|> parseNumber

  -- end of parser --

  getExpr :: String -> Either ParseError LispVal
  getExpr = parse parseExpr "lisp"

  readExpr :: String -> String
  readExpr input = errPrint $ getExpr input

  main :: IO()
  main = do
    (expr:_) <- getArgs
    print $ readExpr $ expr
