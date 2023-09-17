{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( (\\) )
import Control.Monad (replicateM)
 
data Term =
    Var String
    | Abs String Term
    | App Term Term
    | Empty
    deriving (Eq)

instance Show Term where
    show :: Term -> String
    show = showTerm

showTerm :: Term -> String
showTerm (Var x) = x
showTerm (Abs x term) = "(λ" ++ x ++ "." ++ showTerm term ++ ")"
showTerm (App x y) = "(" ++ showTerm x ++ " " ++ showTerm y ++ ")"
showTerm _ = error "Empty detected"

variableSet :: [String]
variableSet = concatMap (\k -> replicateM k ['a'..'z']) [1..]

firstNameAvailable :: [String] -> String
firstNameAvailable = firstNameAvailableRec 0 where
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

sub :: String -> Term -> Term -> Term
sub x v@(Var y) newVal
    | x == y = newVal
    | otherwise = v
sub x a@(Abs y t) newVal
    | x == y = Abs y t
    | notElem y (freeVars newVal) = Abs y (sub x t newVal)
    | otherwise = let y' = firstNameAvailable (freeVars newVal) in Abs y' (sub x (rename y t y') newVal)
sub x (App t1 t2) t = App (sub x t1 t) (sub x t2 t)
sub _ Empty _ = error "Empty detected"

rename :: String -> Term -> String -> Term
rename x v@(Var y) z
    | x == y = Var z
    | otherwise = v
rename x (Abs y t) z
    | x == y = Abs z (rename x t z)
    | otherwise = Abs y (rename x t z)
rename x (App t1 t2) z = App (rename x t1 z) (rename x t2 z)
rename _ Empty _ = error "Empty detected"

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