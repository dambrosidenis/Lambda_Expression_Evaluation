{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( (\\) )
import Control.Monad (replicateM, join)
 
data Term =
    Var String
    | Abs String Term
    | App Term Term
    | Empty
    deriving (Eq)

instance Show Term where
    show :: Term -> String
    show (Var x) = x
    show (Abs x t) = join ["(λ", x, ".", show t, ")"]
    show (App x y) = join ["(", show x, " ", show y, ")"]
    show _ = error "Empty detected"

variableSet :: [String]
variableSet = concatMap (\k -> replicateM k ['a'..'z']) [1..]

firstNameAvailable :: [String] -> String
firstNameAvailable = firstNameAvailableRec 0 where
    firstNameAvailableRec :: Int -> [String] -> String
    firstNameAvailableRec n usedVars
        | elem (variableSet !! n) usedVars = firstNameAvailableRec (n+1) usedVars
        | otherwise = variableSet !! n

eval :: Term -> Term
eval (App (Abs x t) t1) = let t1' = eval t1 in eval (sub x t t1')
eval (App t1 t2) = case eval t1 of
    a@(Abs x t1) -> eval (App a t2)
    t -> App t (eval t2)
eval (Abs x t) = Abs x (eval t)
eval Empty = error "Empty detected"
eval t = t
-- HEAD NORMAL FORM

sub :: String -> Term -> Term -> Term
sub x v@(Var y) newVal
    | x == y = newVal
    | otherwise = v
sub x a@(Abs y t) newVal
    | x == y = a
    | notElem y (freeVars newVal) = Abs y (sub x t newVal)
    | otherwise = let y' = firstNameAvailable (freeVars newVal) in Abs y' (sub x (alphaConv y t y') newVal)
sub x (App t1 t2) t = App (sub x t1 t) (sub x t2 t)
sub _ Empty _ = error "Empty detected"

alphaConv :: String -> Term -> String -> Term
alphaConv x v@(Var y) z
    | x == y = Var z
    | otherwise = v
alphaConv x (Abs y t) z
    | x == y = Abs z (alphaConv x t z)
    | otherwise = Abs y (alphaConv x t z)
alphaConv x (App t1 t2) z = App (alphaConv x t1 z) (alphaConv x t2 z)
alphaConv _ Empty _ = error "Empty detected"

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (Abs x t1) = removeDuplicates (freeVars t1) \\ [x] where
    removeDuplicates :: [String] -> [String]
    removeDuplicates [] = []
    removeDuplicates (x:xs)
        | elem x xs = removeDuplicates xs
        | otherwise = x : removeDuplicates xs
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars _ = error "Empty detected"