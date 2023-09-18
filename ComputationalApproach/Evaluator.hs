{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( (\\), elemIndex )
import Control.Monad (replicateM)
import Text.Read (readMaybe)

data SyntaxTerm = SyntaxVar String
    | SyntaxAbs String SyntaxTerm
    | SyntaxApp SyntaxTerm SyntaxTerm
    | Empty
    deriving (Eq)

instance Show SyntaxTerm where
    show :: SyntaxTerm -> String
    show (SyntaxVar x) = x
    show (SyntaxAbs x term) = "(λ" ++ x ++ "." ++ show term ++ ")"
    show (SyntaxApp x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show _ = error "Empty detected"

data Term = Var String | Abs (Term -> Term) | App Term Term

instance Show Term where
    show :: Term -> String
    show t = show (substInts (toSyntax t))

toSyntax :: Term -> (SyntaxTerm, [String])
toSyntax = toSyntaxRec 0 where
    toSyntaxRec :: Int -> Term -> (SyntaxTerm, [String])
    toSyntaxRec _ (Var x) = case readMaybeInt x of
        Just n -> (SyntaxVar (show n), [])
        Nothing -> (SyntaxVar x, [x])
    toSyntaxRec n (Abs f) = let (term, freeVars) = toSyntaxRec (n+1) (f (Var (show n))) in (SyntaxAbs (show n) term, freeVars)
    toSyntaxRec n (App t1 t2) = let
        (t1', freeVars1) = toSyntaxRec n t1
        (t2', freeVars2) = toSyntaxRec n t2
        in (SyntaxApp t1' t2', removeDuplicates (freeVars1 ++ freeVars2)) where
            removeDuplicates :: [String] -> [String]
            removeDuplicates [] = []
            removeDuplicates (x:xs)
                | elem x xs = removeDuplicates xs
                | otherwise = x : removeDuplicates xs

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

substInts :: (SyntaxTerm, [String]) -> SyntaxTerm
substInts (term, freeVars) = substIntsRec [] term freeVars where
    substIntsRec :: [String] -> SyntaxTerm -> [String] -> SyntaxTerm
    substIntsRec varNames (SyntaxVar x) _ = case readMaybeInt x of
        Just n -> SyntaxVar (varNames !! n)
        Nothing -> SyntaxVar x
    substIntsRec varNames (SyntaxApp t1 t2) usedVars = SyntaxApp (substIntsRec varNames t1 usedVars) (substIntsRec varNames t2 usedVars)
    substIntsRec varNames (SyntaxAbs _ t) usedVars = let x = firstNameAvailable usedVars in SyntaxAbs x (substIntsRec (varNames ++ [x]) t (usedVars ++ [x]))
    substIntsRec _ Empty _ = error "Empty detected"

variableSet :: [String]
variableSet = concatMap (\k -> replicateM k ['a'..'z']) [1..]

firstNameAvailable :: [String] -> String
firstNameAvailable = firstNameAvailableRec 0 where
    firstNameAvailableRec n usedVars
        | elem (variableSet !! n) usedVars = firstNameAvailableRec (n+1) usedVars
        | otherwise = variableSet !! n

lastOccurrence :: (Eq a) => a -> [a] -> Maybe Int
lastOccurrence x xs = foldl (\ acc (new, index) -> if new == x then Just index else acc) Nothing (zip xs [i | i <- [0..]])

eval :: SyntaxTerm -> Term
eval = evalRec [] [] where
    evalRec :: [Term] -> [String] -> SyntaxTerm -> Term
    evalRec env vars (SyntaxAbs v t) = Abs (\ x -> evalRec (env ++ [x]) (vars ++ [v]) t)
    evalRec env vars (SyntaxVar x) = case lastOccurrence x vars of
        Just n -> env !! n
        Nothing -> Var x
    evalRec env vars (SyntaxApp t1 t2) = let t2' = evalRec env vars t2 in case evalRec env vars t1 of
        Abs f -> f t2'
        v@(Var _) -> App v t2'
        a@(App _ _) -> App a t2'
    evalRec _ _ Empty = error "Empty detected"