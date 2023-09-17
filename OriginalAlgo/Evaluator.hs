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
showTerm (App x y) = "(" ++ showTerm x ++ ") (" ++ showTerm y ++ ")"
showTerm _ = error "Empty detected"

eval :: Term -> Term
eval (App (Abs x t) v@(Abs _ _)) = subst x t v
eval (App v1@(Abs _ _) t2) = let t2' = eval t2 in App v1 t2'
eval (App t1 t2) = let t1' = eval t1 in App t1' t2
eval Empty = error "Empty detected"
eval t = t

subst :: String -> Term -> Term -> Term
subst x (Var v) newVal
    | x == v = newVal
    | otherwise = Var v
subst x (Abs y t1) newVal
    | x == y                                  = Abs y t1                -- Nuovo scoping della variabile y
    | x /= y && (y `notElem` freeVars newVal) = Abs y (subst x t1 newVal)
    | otherwise                               = error $ "Cannot substitute '" ++ show x ++ "' in term '" ++ show (Abs y t1) ++ "'"
subst x (App t1 t2) newVal = App (subst x t1 newVal) (subst x t2 newVal)
subst _ _ _ = error "Empty detected"

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