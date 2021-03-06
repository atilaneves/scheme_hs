module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | Character Char
               deriving(Show)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x


escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
             '\\' -> x
             '"' -> x
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _ -> Atom atom


parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseDecimalHash <|> parseOctal <|> parseHex


parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseDecimalHash :: Parser LispVal
parseDecimalHash = try $ string "#d" >> parseDecimal

parseOctal :: Parser LispVal
parseOctal = do try $ string "#o"
                x <- many1 octDigit
                return $ (Number . firstOfFirst . readOct) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ (Number . firstOfFirst . readHex) x


firstOfFirst x = fst $ x !! 0

parseChar :: Parser LispVal
parseChar = do
 try $ string "#\\"
 value <- try (string "newline" <|> string "space")
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
 return $ Character $ case value of
                        "space" -> ' '
                        "newline" -> '\n'
                        otherwise -> (value !! 0)

parseFloat :: Parser LispVal
parseFloat = do
  dec <- many1 digit
  dot <- oneOf "."
  frac <- many1 digit
  return $ (Float . firstOfFirst . readFloat) (dec ++ [dot] ++ frac)


parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces


parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail


parseLists :: Parser LispVal
parseLists = do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseVector :: Parser LispVal
parseVector = do
  string "#("
  values <- parseExpr `sepBy` spaces
  char ')'
  return $ List $ [Atom "vector"] ++ values


parseHash :: Parser LispVal
parseHash = try parseVector <|> parseNumber <|> parseBool <|> parseChar


parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]


parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseHash
        <|> parseQuoted
        <|> parseLists
        <|> parseQuasiQuoted
        <|> parseUnQuote


readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ show err
      Right val -> "Found value of " ++ show val


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
