{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe(a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    mp >>= f = Parser $
        \s -> case parse mp s of
            Nothing -> Nothing
            Just (x, rest) -> parse (f x) rest

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s
                                ok -> ok
--- type declaration over ---

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
  \s -> case s of
          [] -> Nothing
          (x:xs) -> if p x then Just (x, xs) else Nothing

plusParser :: Parser a -> Parser [a]
plusParser p = do 
    x <- p
    whiteSpaceParser
    xs <- starParser p
    return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

variableParser :: Parser String
variableParser = do
    x <- predicateParser isAsciiLower
    xs <- starParser $ predicateParser isAsciiLower
    return (x:xs)

whiteSpaceParser :: Parser String
whiteSpaceParser = starParser (predicateParser (== ' '))

openBracketParser :: Parser String
openBracketParser = starParser (predicateParser (== '('))

closeBracketParser :: Parser String
closeBracketParser = starParser (predicateParser (== ')'))

variableExprParser :: Parser Expr
variableExprParser = Variable <$> variableParser

functionExprParser :: Parser Expr
functionExprParser = do
    openBracketParser
    predicateParser (== '\\')
    v <- variableParser
    predicateParser (== '.')
    res <- Function v <$> simpleExprParser
    closeBracketParser
    return res

applicationBracketParser :: Parser Expr
applicationBracketParser = do
    predicateParser (== '(')
    x <- applicationExprParser
    predicateParser (== ')')
    return x

simpleExprParser :: Parser Expr
simpleExprParser = applicationBracketParser <|> functionExprParser <|> variableExprParser <|> macroExprParser

applicationParser :: Parser Expr
applicationParser = do
    x <- simpleExprParser
    whiteSpaceParser
    y <- plusParser simpleExprParser
    return (foldl Application x y)

applicationExprParser :: Parser Expr
applicationExprParser = applicationBracketParser <|> applicationParser

macroExprParser :: Parser Expr
macroExprParser = do
    predicateParser (== '$')
    Macro <$> variableParser

exprParser :: Parser Expr
exprParser = applicationExprParser <|> functionExprParser <|> variableExprParser <|> macroExprParser

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr str = case parse exprParser str of
    Just (x, xs) -> x
    _ -> error "parse error"

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
