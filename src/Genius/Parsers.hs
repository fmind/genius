module Genius.Parsers where

import           Genius.Types

import           Control.Monad                 (liftM)
import           Control.Monad.Error           (throwError)
import           Text.ParserCombinators.Parsec hiding (spaces)

-- MAIN FUNCTIONS

-- |Generate an expression from a string.
parseExprOrThrow :: String -> ThrowsError LispVal
parseExprOrThrow = parseOrThrow parseExpr

-- |Generate an expression list from a string.
parseExprListOrThrow :: String -> ThrowsError [LispVal]
parseExprListOrThrow = parseOrThrow (endBy parseExpr spaces)

-- |Generate an object from a parser and a string.
parseOrThrow :: Parser a -> String -> ThrowsError a
parseOrThrow parser input = case parse parser "genius" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

-- EXPRESSIONS

-- |Parse an expression.
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

-- |Parse an atom value.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
              "#t" -> Bool True
              "#f" -> Bool False
              _    -> Atom atom

-- |Parse a string value.
parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x

-- |Parse a number value.
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- |Parse a list of values.
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- |Parse a dotted list of values.
parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

-- |Parse a quoted expression.
parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- TOKENS

-- |Parse a symbol.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- |Parse spaces.
spaces :: Parser ()
spaces = skipMany1 space
