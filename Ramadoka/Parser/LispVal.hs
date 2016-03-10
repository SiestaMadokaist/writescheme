{-# LANGUAGE QuasiQuotes #-}
module Ramadoka.Parser.LispVal
(
  getExpr,
  normalizeRational,
  LispVal(..),
  Number(..),
  ThrowsError,
  eval,
) where
  import Control.Monad as M
  import Control.Monad.Error
  import Data.List
  import Data.String.Interpolate
  import Ramadoka.Parser.Number
  import System.Environment
  import Text.ParserCombinators.Parsec hiding (spaces)

  data LispError = NumArgs Integer [LispVal]
                  | TypeMismatch String LispVal
                  | ParseError ParseError
                  | BadSpecialForm String LispVal
                  | NotFunction String String
                  | UnboundVar String String
                  | Default String
                  deriving (Eq)

  data LispVal = Number Number
    | Bool Bool
    | Atom String
    | Char Char
    | String String
    | List [LispVal]
    | DottedList [LispVal]
    | Failure LispError
    deriving (Eq)

  instance Show LispVal where
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Char c) = show c
    show (String s) = [i|`#{s}`|]
    show (List l) = [i|(#{stringify l})|]
    show (DottedList l) = [i|Dotted (#{stringify l})|]
    show (Number n) = show n
    show (Atom s) = [i|Atom #{s}|]
    show (Failure s) = [i|Failure #{s}|]

  stringify :: [LispVal] -> String
  stringify xs = ((intercalate ", ") . (map show)) xs

  showError :: LispError -> String
  showError (UnboundVar message varname) = [i|#{message}: #{varname}|]
  showError (BadSpecialForm message form) = [i|#{message}: #{form}|]
  showError (NotFunction message func) = [i|#{message}: #{func}|]
  showError (NumArgs expected found) = [i|Expected: #{expected} args; found values: #{stringify found}|]
  showError (TypeMismatch expected found) = [i|Invalid Type, expected: #{expected}, found: #{found}|]
  showError (ParseError parseErr) = [i|Parse error at #{parseErr}|]

  instance Show LispError where show = showError

  instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

  type ThrowsError = Either LispError

  trapError :: (Show a, MonadError a m) => m String -> m String
  trapError action = catchError action (return . show)

  extractValue :: ThrowsError a -> a
  extractValue (Right val) = val

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

  parseFloat :: Integer -> Parser LispVal
  parseFloat i = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read ("0." ++ xs)
        currentResult = return $ Number $ Float (fromIntegral i + mantissa)
    (parseFloatPower $ fromIntegral i + mantissa) <|> currentResult

  parseIntPower :: Integer -> Parser LispVal
  parseIntPower base = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ Number $ Rational ( base * ( 10 ^ exponent ) ) 1

  parseFloatPower :: Float -> Parser LispVal
  parseFloatPower base = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Float
    return $ Number $ Float ( base * ( 10 ** exponent ) )

  parseNumber :: Parser LispVal
  parseNumber = do
    xs <- many1 digit
    let currentNumber = read xs :: Integer
        base = currentNumber
        numerator = currentNumber
        characteristic = currentNumber
    (parseRational numerator) <|> (parseFloat characteristic) <|> (parseIntPower base) <|> (return $ Number $ Rational characteristic 1)

  parseExactPoweredFloat :: Integer -> Integer -> Parser LispVal
  parseExactPoweredFloat numerator denominator = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ Number $ normalizeRational (numerator * 10 ^ exponent) (denominator)

  parseExactFloat :: Integer -> Parser LispVal
  parseExactFloat characteristic = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read xs :: Integer
        exponent = length xs
        denominator = 10 ^ exponent
        numerator = ((characteristic * denominator) + mantissa)
    (parseExactPoweredFloat numerator denominator) <|> (return $ Number $ normalizeRational numerator denominator)

  parseRational :: Integer -> Parser LispVal
  parseRational numerator = do
    xs <- digitsDirectlyAfter '/'
    let denominator = read xs :: Integer
    return $ Number $ normalizeRational numerator denominator

  parseExactNumber :: Parser LispVal
  parseExactNumber = do
    xs <- digitsDirectlyAfter 'e'
    let characteristic = read xs :: Integer
        numerator = characteristic
    (parseRational numerator) <|> (parseIntPower characteristic) <|> (parseExactFloat characteristic) <|> (return $ Number $ Rational characteristic 1)

  parseInexactNumber :: Parser LispVal
  parseInexactNumber = do
    xs <- digitsDirectlyAfter 'i'
    let characteristic = read xs :: Float
        directResult = return $ Number $ Float characteristic
    (parseFloatPower characteristic) <|> (parseFloat $ floor characteristic) <|> (directResult)

  parseTrue :: Parser LispVal
  parseTrue = do
    char 't'
    return $ Bool True

  parseFalse :: Parser LispVal
  parseFalse = do
    char 'f'
    return $ Bool False

  parseAtom :: Parser LispVal
  parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
        in return $ Atom atom

  parseSymbolic :: Parser LispVal
  parseSymbolic = do
    char '#'
    parseTrue <|> parseFalse <|> parseExactNumber <|> parseInexactNumber

  parseString :: Parser LispVal
  parseString = do
    char '"'
    xs <- many (escapeCharacters <|> noneOf "\"\\")
    char '"'
    return $ String xs

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
    return $ DottedList exprs

  parseQuoted :: Parser LispVal
  parseQuoted = do
    char '`'
    expr <- parseExpr
    return $ List [Atom "quote", expr]

  parseExpr :: Parser LispVal
  parseExpr = parseSymbolic <|> (parseFloat 0) <|> parseNumber <|> parseString <|> parseAtom <|> parseQuoted <|> (try parseList) <|> (try parseDottedList)

  -- end of parser --
  eval :: LispVal -> ThrowsError LispVal
  eval val@(Bool _) = return val
  eval val@(Number _) = return val
  eval val@(String _) = return val
  eval (List [Atom "quote", expr]) = return expr
  eval (List [Atom "if", pred, cons, alt]) = (eval pred) >>= (\x -> evalIf x cons alt)
  eval (List (Atom func : exprs)) = mapM eval exprs >>= apply func
  eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

  evalIf :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
  evalIf (Bool True)  cons _ = eval cons
  evalIf (Bool False)  _ alt = eval alt

  apply :: String -> [LispVal] -> ThrowsError LispVal
  apply "+" = numericBinOp (|+|)
  apply "-" = numericBinOp (|-|)
  apply "*" = numericBinOp (|*|)
  apply "/" = numericBinOp (|/|)
  apply "&&" = boolBoolBinOp (&&)
  apply "||" = boolBoolBinOp (||)
  apply ">" = numBoolBinOp (>)
  apply "<" = numBoolBinOp (<)
  apply "==" = numBoolBinOp (==)
  apply ">=" = numBoolBinOp (>=)
  apply "<=" = numBoolBinOp (<=)
  apply "string<?" = strBoolBinOp (<)
  apply "string=?" = strBoolBinOp (==)
  apply "string>?" = strBoolBinOp (>)
  apply "string>=?" = strBoolBinOp (>=)
  apply "string<=?" = strBoolBinOp (<=)
  apply "string?" = return . isString . head
  apply "number?" = return . isNumber . head
  apply "symbol?" = return . isSymbol . head

  strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
  strBoolBinOp op [] = throwError $ NumArgs 2 []
  strBoolBinOp op singleVal@(_:[]) = throwError $ NumArgs 2 singleVal
  strBoolBinOp op (s1:s2:[]) = do
    ss1 <- unpackStr s1
    ss2 <- unpackStr s2
    return $ Bool $ op ss1 ss2

  numBoolBinOp :: (Number -> Number -> Bool) -> [LispVal] -> ThrowsError LispVal
  numBoolBinOp op [] = throwError $ NumArgs 2 []
  numBoolBinOp op singleVal@(x:[]) = throwError $ NumArgs 2 singleVal
  numBoolBinOp op (x':y':[]) = do
    x <- unpackNum x'
    y <- unpackNum y'
    return $ Bool $ op x y
  numBoolBinOp op multiVal@(_:_:_) = throwError $ NumArgs 2 multiVal

  numericBinOp :: (Number -> Number -> Number) -> [LispVal] -> ThrowsError LispVal
  numericBinOp op [] = throwError $ NumArgs 2 []
  numericBinOp op singleVal@(x:[]) = throwError $ NumArgs 2 singleVal
  numericBinOp op args = mapM unpackNum args >>= return . Number . foldl1 op

  boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
  boolBoolBinOp op [] = throwError $ NumArgs 2 []
  boolBoolBinOp op singleVal@(x:[]) = throwError $ NumArgs 2 singleVal
  boolBoolBinOp op xs = mapM unpackBool xs >>= return . Bool . foldl1 op

  unpackStr :: LispVal -> ThrowsError String
  unpackStr (String s) = Right s
  unpackStr notString = throwError $ TypeMismatch "string" notString

  unpackNum :: LispVal -> ThrowsError Number
  unpackNum (Number r@(Rational _ _)) = Right r
  unpackNum (Number f@(Float _)) = Right f
  unpackNum notNumber = throwError $ TypeMismatch "number" notNumber

  unpackBool :: LispVal -> ThrowsError Bool
  unpackBool (Bool x) = (Right x)

  isSymbol :: LispVal -> LispVal
  isSymbol (Atom _) = Bool True
  isSymbol _ = Bool False

  isString :: LispVal -> LispVal
  isString (String _) = Bool True
  isString _ = Bool False

  isNumber :: LispVal -> LispVal
  isNumber (Number _) = Bool True
  isNumber _ = Bool False

  getExpr :: String -> ThrowsError LispVal
  getExpr input = handleParseError $ parse parseExpr "lisp" input

  handleParseError :: Either ParseError LispVal -> ThrowsError LispVal
  handleParseError (Left err) = throwError $ ParseError err
  handleParseError (Right val) = return val

  errPrint :: (Show a) => Either a LispVal -> String
  errPrint (Left err) = [i|No Match: #{err}|]
  errPrint (Right val) = [i|Found Value: #{val}|]

  main :: IO()
  main = do
    (expr:_) <- getArgs
    print $ mapM eval (getExpr expr)

