module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
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


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool


readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ show err
      Right val -> "Found value of " ++ show val


main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
