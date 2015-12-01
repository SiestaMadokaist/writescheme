import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List
import System.Environment
import Control.Monad as M

data JsonKey = KString String
             deriving (Show)

data JsonValue = VString String
               | Integer Integer
               | Json JsonValue
               | KeyValuePair [(JsonKey, JsonValue)]
               deriving (Show)

showVal :: JsonValue -> String
showVal (VString s) = s
showVal (Json j) = showVal j
showVal (KeyValuePair xs) = (intercalate ", ") . (map printPair) $ xs

printPair :: (JsonKey, JsonValue) -> String
printPair (KString k, v) = "(" ++ k ++ " : " ++ showVal v ++ ")"

parseVString :: Parser JsonValue
parseVString = do
        char '\''
        entry <- many (letter <|> digit)
        char '\''
        return $ VString entry

parseKString :: Parser JsonKey
parseKString = do
        char '\''
        entry <- many (letter <|> digit)
        char '\''
        return $ KString entry

parseJValue :: Parser JsonValue
parseJValue = do
        parseVString <|> parseJson

parseKeyValue :: Parser (JsonKey, JsonValue)
parseKeyValue = do
        key <- parseKString
        char ':'
        value <- parseJValue
        char ',' <|> char '}'
        return $ (key, value)

parseJson :: Parser JsonValue
parseJson = do
        char '{'
        keyValuePair <- many parseKeyValue
        return $ KeyValuePair keyValuePair

errPrint :: (Show a) => Either a JsonValue -> String
errPrint (Left err) = "No match:" ++ show err
errPrint (Right val) = showVal val

readExpr :: String -> String
readExpr input = errPrint $ parse parseJson "json" input
