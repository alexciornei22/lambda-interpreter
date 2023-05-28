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
    xs <- starParser p
    return (x:xs)

starParser :: Parser a -> Parser [a]
starParser p = plusParser p <|> return []

variableParser :: Parser String
variableParser = do
    x <- predicateParser isAsciiLower
    return [x]

macroParser :: Parser String
macroParser = do
    x <- predicateParser isAsciiLower
    xs <- starParser $ predicateParser isAsciiLower
    return (x:xs)

whiteSpaceParser :: Parser String
whiteSpaceParser = starParser (predicateParser (== ' '))

variableExprParser :: Parser Expr
variableExprParser = Variable <$> variableParser

functionExprParser :: Parser Expr
functionExprParser = do
    predicateParser (== '\\')
    v <- variableParser
    predicateParser (== '.')
    res <- Function v <$> simpleExprParser
    return res

simpleExprParser :: Parser Expr
simpleExprParser = exprBracketParser <|> functionExprParser <|> variableExprParser <|> macroExprParser

applicationParser :: Parser Expr
applicationParser = do
    x <- simpleExprParser
    y <- plusParser (do
        whiteSpaceParser
        simpleExprParser
        )
    return (foldl Application x y)

applicationExprParser :: Parser Expr
applicationExprParser = applicationParser

macroExprParser :: Parser Expr
macroExprParser = do
    predicateParser (== '$')
    Macro <$> macroParser

exprBracketParser :: Parser Expr
exprBracketParser = do
    predicateParser (== '(')
    res <- exprParser
    predicateParser (== ')')
    return res

exprParser :: Parser Expr
exprParser = applicationExprParser <|> exprBracketParser <|> functionExprParser <|> variableExprParser <|> macroExprParser

-- TODO 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr str = case parse exprParser str of
    Just (x, xs) -> x
    _ -> Variable "parse error"

-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code = undefined
