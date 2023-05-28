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
getVars :: Expr -> [String]
getVars (Variable x) = [x]
getVars (Function x ex) = nub (getVars ex)
getVars (Application ex1 ex2) = nub (getVars ex1 ++ getVars ex2)

getSubstituteAux :: Int -> [String] -> String
getSubstituteAux x vars =
    if show x `elem` vars then
        getSubstituteAux (x + 1) vars
    else
        show x

reduce :: Expr -> String -> Expr -> Expr
reduce ex1 x ex2 = case ex1 of
    Variable var -> if var == x then ex2 else ex1
    Application app1 app2 -> Application (reduce app1 x ex2) (reduce app2 x ex2)
    Function var fun -> if var == x then
        Function var fun
        else if var `elem` free_vars ex2 then Function (getSubstitute fun) (reduce (reduce fun var (Variable "1")) x ex2)
        else Function var (reduce fun x ex2)
    where
        getSubstitute :: Expr -> String
        getSubstitute ex = getSubstituteAux 1 (getVars ex)

is_redex :: Expr -> Bool
is_redex (Application (Function var ex1) ex2) = True
is_redex (Application ex1 ex2) = is_redex ex1 || is_redex ex2
is_redex (Function var ex) = is_redex ex
is_redex _ = False

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function var ex1) ex2) = reduce ex1 var ex2
stepN (Application ex1 ex2) =
    if is_redex ex1 then
        Application (stepN ex1) ex2
    else
        Application ex1 (stepN ex2)
stepN (Function var ex) = Function var $ stepN ex
stepN ex = ex

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN ex = if is_redex ex then reduceN $ stepN ex else ex

reduceAllN :: Expr -> [Expr]
reduceAllN ex = if is_redex ex then ex : reduceAllN (stepN ex) else [ex]

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Application (Function var ex1) ex2)
    | is_redex ex1 = Application (Function var $ stepA ex1) ex2
    | is_redex ex2 = Application (Function var ex1) (stepA ex2)
    | otherwise = reduce ex1 var ex2
stepA (Application ex1 ex2) =
    if is_redex ex1 then
        Application (stepA ex1) ex2
    else
        Application ex1 (stepA ex2)
stepA (Function var ex) = Function var $ stepA ex
stepA ex = ex

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA ex = if is_redex ex then reduceA $ stepA ex else ex

reduceAllA :: Expr -> [Expr]
reduceAllA ex = if is_redex ex then ex : reduceAllA (stepA ex) else [ex]

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dict ex = case ex of
    Macro m -> case findMacro m of
        Just x -> x
        _ -> error "invalid macro"
    Function var fun -> Function var (evalMacros dict fun)
    Application app1 app2 -> Application (evalMacros dict app1) (evalMacros dict app2)
    _ -> ex
    where
        findMacro name = case lookup name dict of
            Just (Macro m) -> findMacro m
            ok -> ok

-- TODO 4.1. evaluate code sequence using given strategy
evalHelper :: (Expr -> Expr) -> [(String, Expr)] -> [Code] -> [Expr]
evalHelper strategy dict codes = case codes of
    [] -> []
    (x:xs) -> case x of
        Assign m ex -> evalHelper strategy ((m, ex) : dict) xs
        Evaluate ex -> strategy (evalMacros dict ex) : evalHelper strategy dict xs

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy = evalHelper strategy []
