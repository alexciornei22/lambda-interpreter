{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Expr
import Data.List
import Foreign (free)

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x ex) = nub (filter (/= x) (free_vars ex))
free_vars (Application ex1 ex2) = nub (free_vars ex1 ++ free_vars ex2)

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce ex1 x ex2 = case ex1 of
    Variable var -> if var == x then ex2 else ex1
    Application app1 app2 -> Application (reduce app1 x ex2) (reduce app2 x ex2)
    Function var fun -> if var == x then
        Function var fun
        else if var `elem` free_vars ex2 then Function "a" (reduce (reduce fun var (Variable "a")) x ex2)
        else Function var (reduce fun x ex2)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN = undefined

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN = undefined

reduceAllN :: Expr -> [Expr]
reduceAllN = undefined

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA = undefined

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA = undefined

reduceAllA :: Expr -> [Expr]
reduceAllA = undefined

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
