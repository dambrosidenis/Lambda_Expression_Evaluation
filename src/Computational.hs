{-# LANGUAGE InstanceSigs #-}
module Computational where

import Data.List ( (\\), elemIndex )
import Control.Monad (replicateM, join)
import Text.Read (readMaybe)
import LambdaTerm

data CompTerm = CompVar String
    | CompAbs (CompTerm -> CompTerm)
    | CompApp CompTerm CompTerm

instance Show CompTerm where
    show :: CompTerm -> String
    show t = show (substInts (toSyntax t))

toSyntax :: CompTerm -> (Term, [String])
toSyntax = toSyntaxRec 0 where
    toSyntaxRec :: Int -> CompTerm -> (Term, [String])
    toSyntaxRec _ (CompVar x) = case readMaybeInt x of
        Just n -> (Var (show n), [])
        Nothing -> (Var x, [x])
    toSyntaxRec n (CompAbs f) = let (term, freeVars) = toSyntaxRec (n+1) (f (CompVar (show n))) in (Abs (show n) term, freeVars)
    toSyntaxRec n (CompApp t1 t2) = let
                (t1', freeVars1) = toSyntaxRec n t1
                (t2', freeVars2) = toSyntaxRec n t2
            in (App t1' t2', removeDuplicates (freeVars1 ++ freeVars2)) where
                removeDuplicates :: [String] -> [String]
                removeDuplicates [] = []
                removeDuplicates (x:xs)
                    | elem x xs = removeDuplicates xs
                    | otherwise = x : removeDuplicates xs

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

substInts :: (Term, [String]) -> Term
substInts (term, freeVars) = substIntsRec [] term freeVars where
    substIntsRec :: [String] -> Term -> [String] -> Term
    substIntsRec varNames (Var x) _ = case readMaybeInt x of
        Just n -> Var (varNames !! n)
        Nothing -> Var x
    substIntsRec varNames (App t1 t2) usedVars = App (substIntsRec varNames t1 usedVars) (substIntsRec varNames t2 usedVars)
    substIntsRec varNames (Abs _ t) usedVars = let x = firstNameAvailable usedVars in Abs x (substIntsRec (varNames ++ [x]) t (usedVars ++ [x]))
    substIntsRec _ Empty _ = error "Empty detected"

variableSet :: [String]
variableSet = concatMap (\k -> replicateM k ['a'..'z']) [1..]

firstNameAvailable :: [String] -> String
firstNameAvailable = firstNameAvailableRec 0 where
    firstNameAvailableRec n usedVars
        | elem (variableSet !! n) usedVars = firstNameAvailableRec (n+1) usedVars
        | otherwise = variableSet !! n

eval :: Term -> CompTerm
eval = evalRec [] [] where
    evalRec :: [CompTerm] -> [String] -> Term -> CompTerm
    evalRec env vars (Abs v t) = CompAbs (\ x -> evalRec (x:env) (v:vars) t)
    evalRec env vars (Var x) = case elemIndex x vars of
        Just n -> env !! n
        Nothing -> CompVar x
    evalRec env vars (App t1 t2) = let t2' = evalRec env vars t2 in case evalRec env vars t1 of
        CompAbs f -> f t2'
        v@(CompVar _) -> CompApp v t2'
        a@(CompApp _ _) -> CompApp a t2'
    evalRec _ _ Empty = error "Empty detected"

lastOccurrence :: (Eq a) => a -> [a] -> Maybe Int
lastOccurrence x xs = foldl (\ acc (new, index) -> if new == x then Just index else acc) Nothing (zip xs [i | i <- [0..]])