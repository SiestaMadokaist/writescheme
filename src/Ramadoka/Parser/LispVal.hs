{-# LANGUAGE QuasiQuotes #-}
module Ramadoka.Parser.LispVal
(
  readExpr,
  normalizeRational,
  LispVal(..),
  Number(..),
  ThrowsError,
  eval,
) where
  import Control.Monad as M
  import Data.IORef
  import Control.Monad.Error
  import Data.List
  -- import Data.String.Interpolate
  import Ramadoka.Parser.Number
  import System.Environment
  import Text.ParserCombinators.Parsec hiding (spaces)
  import System.IO

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
    | DottedList [LispVal] LispVal
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
    | Failure LispError

  instance Eq LispVal where
    PrimitiveFunc _ == PrimitiveFunc _          = False
    Func _ _ _ _ == Func _ _ _ _                = False
    Atom a == Atom b                            = a == b
    Bool a == Bool b                            = a == b
    String a == String b                        = a == b
    List xs == List ys                          = xs == ys
    DottedList xs xtail == DottedList ys ytail  = xs == ys
    Char a == Char b                            = a == b
    Failure a == Failure b                      = a == b
    _ == _                                      = False

  type Env = IORef [(String, IORef LispVal)]

  nullEnv :: IO Env
  nullEnv = newIORef []

  runIOThrows :: IOThrowsError String -> IO String
  runIOThrows action = runErrorT (trapError action) >>= return . extractValue

  -- instance Show LispVal where
  showVal :: LispVal -> String
  showVal (Bool True) = "#t"
  showVal (Bool False) = "#f"
  showVal (Char c) = show c
  showVal (String s) = "`" ++ s ++ "`"
  showVal (List l) = "(" ++ stringify l ++ ")"
  showVal (DottedList l v) = "Dotted (" ++ stringify l ++  "+" ++  show v ++ ")"
  -- show (List l) = show l
  -- show (DottedList l v) = show (l, v)
  showVal (Number n) = show n
  showVal (Atom name) = name
  showVal (Failure s) = "Failure " ++ show s
  showVal (PrimitiveFunc _) = "<primitive>"
  showVal (Func _ _ _ _)  = "lambda:... "

  instance Show LispVal where show = showVal

  stringify :: [LispVal] -> String
  stringify = intercalate ", " . map show

  showError :: LispError -> String
  showError (UnboundVar message varname) = message ++ ": "  ++ varname
  showError (BadSpecialForm message form) = message ++ ": "  ++ show form
  showError (NotFunction message func) = message ++ ": "  ++ show func
  showError (NumArgs expected found) = "Expected: " ++ show expected ++ "args; found values: " ++ stringify found
  showError (TypeMismatch expected found) = "Invalid Type, expected: " ++ show expected ++ ", found: " ++ show found
  showError (ParseError parseErr) = "Parse error at " ++ show parseErr

  isBound :: Env -> String -> IO Bool
  isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

  getVar :: Env -> String -> IOThrowsError LispVal
  getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "unbound variable" var) (liftIO . readIORef) (lookup var env)

  unliftIORef :: (String, IORef LispVal) -> IO (String, LispVal)
  unliftIORef (var, valRef) = (readIORef valRef >>= \x -> return (var, x))

  showVar :: Env -> IO [(String, LispVal)]
  showVar envRef = do
    env <- readIORef envRef
    mapM unliftIORef env

  setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
  setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "setting undefined var" var)
          (liftIO . flip writeIORef value)
          (lookup var env)
    return value

  defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
  defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) :env)
        return value

  bindVars :: Env -> [(String, LispVal)] -> IO Env
  bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBindings bindings)
          addBindings (var, value) = newIORef value >>= \val -> return (var, val)

  primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
  primitives = [
      ("+", numericBinOp (|+|)),
      ("-", numericBinOp (|-|)),
      ("*", numericBinOp (|*|)),
      ("/", numericBinOp (|/|)),
      ("&&", boolBoolBinOp (&&)),
      ("car", car),
      ("cdr", cdr),
      ("cons", cons),
      ("||", boolBoolBinOp (||)),
      (">", numBoolBinOp (>)),
      ("<", numBoolBinOp (<)),
      ("==", numBoolBinOp (==)),
      (">=", numBoolBinOp (>=)),
      ("<=", numBoolBinOp (<=)),
      ("string<?", strBoolBinOp (<)),
      ("string=?", strBoolBinOp (==)),
      ("string>?", strBoolBinOp (>)),
      ("string>=?", strBoolBinOp (>=)),
      ("string<=?", strBoolBinOp (<=)),
      ("string?", return . isString . head),
      ("number?", return . isNumber . head),
      ("symbol?", return . isSymbol . head)
    ]

  primitiveBindings :: IO Env
  primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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
    oneOf "\\\"nrt"

  symbol :: Parser Char
  symbol = oneOf "!$%&|*+-/:<=>?@^_-"

  digitsDirectlyAfter :: Char -> Parser String
  digitsDirectlyAfter c = do
    char c
    many1 digit

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
    parseFloatPower (fromIntegral i + mantissa) <|> currentResult

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
    parseRational numerator <|> parseFloat characteristic <|> parseIntPower base <|> return (Number $ Rational characteristic 1)

  parseExactPoweredFloat :: Integer -> Integer -> Parser LispVal
  parseExactPoweredFloat numerator denominator = do
    xs <- digitsDirectlyAfter 'e'
    let exponent = read xs :: Integer
    return $ Number $ normalizeRational (numerator * 10 ^ exponent) denominator

  parseExactFloat :: Integer -> Parser LispVal
  parseExactFloat characteristic = do
    xs <- digitsDirectlyAfter '.'
    let mantissa = read xs :: Integer
        exponent = length xs
        denominator = 10 ^ exponent
        numerator = characteristic * denominator + mantissa
    parseExactPoweredFloat numerator denominator <|> return (Number $ normalizeRational numerator denominator)

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
    parseRational numerator <|> parseIntPower characteristic <|> parseExactFloat characteristic <|> return (Number $ Rational characteristic 1)

  parseInexactNumber :: Parser LispVal
  parseInexactNumber = do
    xs <- digitsDirectlyAfter 'i'
    let characteristic = read xs :: Float
        directResult = return $ Number $ Float characteristic
    parseFloatPower characteristic <|> parseFloat (floor characteristic) <|> directResult

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

  parseListInner :: Parser LispVal
  parseListInner = do
    exprs <- try (sepBy parseExpr spaces)
    return $ List exprs

  parseList :: Parser LispVal
  parseList = do
    char '('
    exprs <- parseListInner <|> parseDottedList
    char ')'
    return exprs

  parseDottedList :: Parser LispVal
  parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '~' >> spaces >> parseExpr
    return $ DottedList head tail

  parseQuoted :: Parser LispVal
  parseQuoted = do
    char '`'
    expr <- parseExpr
    return $ List [Atom "quote", expr]

  parseExpr :: Parser LispVal
  parseExpr = parseSymbolic <|> parseFloat 0 <|> parseNumber <|> parseString <|> parseAtom <|> parseQuoted <|> parseList

  makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
  makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

  makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
  makeNormalFunc = makeFunc Nothing

  makeVarArgs = makeFunc . Just . showVal

  -- end of parser --
  eval :: Env -> LispVal -> IOThrowsError LispVal
  eval env val@(Bool _) = return val
  eval env val@(Number _) = return val
  eval env val@(String _) = return val
  eval env (Atom varname) = getVar env varname
  eval env (List [Atom "quote", expr]) = return expr
  eval env (List [Atom "if", pred, cons, alt]) = eval env pred >>= (\result -> evalIf env result cons alt)
  eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
  eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
  eval env (List (Atom "define" : List (Atom var : params) : body)) =
         makeNormalFunc env params body >>= defineVar env var
  -- eval env (List [Atom "define", List (Atom funcName:params), body]) = makeNormalFunc env params body >>= defineVar env funcName
  eval env (List (function : args)) = do
    func <- eval env function
    argValues <- mapM (eval env) args
    apply func argValues
  eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

  evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
  evalIf env (Bool True)  cons _ = eval env cons
  evalIf env (Bool False)  _ alt = eval env alt

  apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
  apply (PrimitiveFunc func) args = liftThrows $ func args
  apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else do
           localEnv <- liftIO $ bindVars closure $ zip params args
           bindVarArgs varargs localEnv
           evalBody localEnv
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env

  car :: [LispVal] -> ThrowsError LispVal
  car [List (x:xs)] = return x
  car [DottedList (x:xs) _ ] = return $ List xs
  car [notList] = throwError $ TypeMismatch "list" notList
  -- car badArgsList = throwError $ NumArgs 1 badArgsList

  cdr :: [LispVal] -> ThrowsError LispVal
  cdr [List (x:xs)] = return $ List xs
  cdr [DottedList (_:xs) y] = return $ DottedList xs y
  cdr [DottedList [_] y] = return y
  cdr [notList] = throwError $ TypeMismatch "list" notList
  cdr badArgsList = throwError $ NumArgs 1 badArgsList

  cons :: [LispVal] -> ThrowsError LispVal
  cons [x, List xs] = return $ List (x:xs)
  cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
  cons [x1, x2] = return $ DottedList [x1] x2
  cons badArgsList = throwError $ NumArgs 2 badArgsList

  strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
  strBoolBinOp op [] = throwError $ NumArgs 2 []
  strBoolBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
  strBoolBinOp op [s1, s2] = do
    ss1 <- unpackStr s1
    ss2 <- unpackStr s2
    return $ Bool $ op ss1 ss2

  numBoolBinOp :: (Number -> Number -> Bool) -> [LispVal] -> ThrowsError LispVal
  numBoolBinOp op [] = throwError $ NumArgs 2 []
  numBoolBinOp op singleVal@[x] = throwError $ NumArgs 2 singleVal
  numBoolBinOp op [x', y'] = do
    x <- unpackNum x'
    y <- unpackNum y'
    return $ Bool $ op x y
  numBoolBinOp op multiVal@(_:_:_) = throwError $ NumArgs 2 multiVal

  numericBinOp :: (Number -> Number -> Number) -> [LispVal] -> ThrowsError LispVal
  numericBinOp op [] = throwError $ NumArgs 2 []
  numericBinOp op singleVal@[x] = throwError $ NumArgs 2 singleVal
  numericBinOp op args = liftM (Number . foldl1 op) $ mapM unpackNum args

  boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
  boolBoolBinOp op [] = throwError $ NumArgs 2 []
  boolBoolBinOp op singleVal@[x] = throwError $ NumArgs 2 singleVal
  boolBoolBinOp op xs = liftM (Bool . foldl1 op) $ mapM unpackBool xs

  unpackStr :: LispVal -> ThrowsError String
  unpackStr (String s) = Right s
  unpackStr notString = throwError $ TypeMismatch "string" notString

  unpackNum :: LispVal -> ThrowsError Number
  unpackNum (Number r@(Rational _ _)) = Right r
  unpackNum (Number f@(Float _)) = Right f
  unpackNum notNumber = throwError $ TypeMismatch "number" notNumber

  unpackBool :: LispVal -> ThrowsError Bool
  unpackBool (Bool x) = Right x

  isSymbol :: LispVal -> LispVal
  isSymbol (Atom _) = Bool True
  isSymbol _ = Bool False

  isString :: LispVal -> LispVal
  isString (String _) = Bool True
  isString _ = Bool False

  isNumber :: LispVal -> LispVal
  isNumber (Number _) = Bool True
  isNumber _ = Bool False

  type IOThrowsError = ErrorT LispError IO

  readExpr :: String -> ThrowsError LispVal
  readExpr input = handleParseError $ parse parseExpr "lisp" input

  liftThrows :: ThrowsError a -> IOThrowsError a
  liftThrows (Left err) = throwError err
  liftThrows (Right val) = return val

  evalLisp :: Env -> ThrowsError LispVal -> IO String
  evalLisp env expr = runIOThrows $ liftM show $ (liftThrows expr) >>= eval env

  evalString ::  Env -> String -> IO String
  evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

  handleParseError :: Either ParseError LispVal -> ThrowsError LispVal
  handleParseError (Left err) = throwError $ ParseError err
  handleParseError (Right val) = return val

  flushStr :: String -> IO()
  flushStr str = putStr str >> hFlush stdout

  readPrompt :: String -> IO String
  readPrompt prompt = flushStr prompt >> getLine

  evalAndPrint :: Env -> String -> IO()
  evalAndPrint env expr = evalString env expr >>= putStrLn

  until_ :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
  until_ pred prompt action = do
    result <- prompt
    unless (pred result) $ action result >> until_ pred prompt action

  errPrint :: (Show a) => Either a LispVal -> String
  errPrint (Left err) = "No Match:" ++ show err
  errPrint (Right val) = "Found Value:" ++ show val

  runRepl :: IO()
  runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>>") . evalAndPrint

  runOne :: String -> IO()
  runOne expr = primitiveBindings >>= flip evalAndPrint expr

  main :: IO()
  main = do
     args <- getArgs
     case length args of
       0 -> runRepl
       1 -> runOne $ args !! 0
