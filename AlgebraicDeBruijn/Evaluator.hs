{-# LANGUAGE InstanceSigs #-}
module Evaluator where
import Data.List (elemIndex)
 
data Term = Var String
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

data DBTerm = DBVar Int
    | DBConst String
    | DBAbs DBTerm
    | DBApp DBTerm DBTerm
    | DBEmpty
    deriving (Eq)

instance Show DBTerm where
    show :: DBTerm -> String
    show = showDBTerm

showDBTerm :: DBTerm -> String
showDBTerm (DBVar n) = show n
showDBTerm (DBConst x) = x
showDBTerm (DBAbs t) = "(λ " ++ showDBTerm t ++ ")"
showDBTerm (DBApp t1 t2) = "(" ++ showDBTerm t1 ++ " " ++ showDBTerm t2 ++ ")"
showDBTerm DBEmpty = error "DBEmpty detected"

lastOccurrence :: (Eq a) => a -> [a] -> Maybe Int
lastOccurrence x xs = foldl (\ acc (new, index) -> if new == x then Just index else acc) Nothing (zip xs [i | i <- [0..]])

toDeBruijn :: Term -> DBTerm
toDeBruijn = toDeBruijnRec [] where
    toDeBruijnRec :: [String] -> Term -> DBTerm
    toDeBruijnRec vars (Var x) = case elemIndex x vars of
        Just n -> DBVar (n+1)
        Nothing -> DBConst x
    toDeBruijnRec vars (Abs x t) = DBAbs (toDeBruijnRec (x:vars) t)
    toDeBruijnRec vars (App t1 t2) = DBApp (toDeBruijnRec vars t1) (toDeBruijnRec vars t2)
    toDeBruijnRec _ Empty = error "DBEmpty detected"

eval :: Term -> DBTerm
eval t = dbEval (toDeBruijn t)

dbEval :: DBTerm -> DBTerm
dbEval (DBApp (DBAbs t) t1) = sub t t1 1
dbEval (DBApp t1 t2) = let t1' = dbEval t1 in DBApp t1' t2
dbEval DBEmpty = error "DBEmpty detected"
dbEval t = t

sub :: DBTerm -> DBTerm -> Int -> DBTerm
sub (DBVar n) t n'
    | n == n' = t
    | otherwise = DBVar n
sub c@(DBConst _) _ _ = c
sub (DBAbs t1) t2 n = DBAbs (sub t1 t2 (n+1))
sub (DBApp t1 t2) t n = DBApp (sub t1 t n) (sub t2 t n)
sub _ _ _ = error "DBEmpty detected"