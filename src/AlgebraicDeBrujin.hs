{-# LANGUAGE InstanceSigs #-}
module AlgebraicDeBrujin where

import Data.List (elemIndex)
import Control.Monad (join)
import LambdaTerm

data DBTerm = DBVar Int
    | DBConst String
    | DBAbs DBTerm
    | DBApp DBTerm DBTerm
    deriving (Eq)

instance Show DBTerm where
    show :: DBTerm -> String
    show (DBVar n) = Prelude.show n
    show (DBConst x) = x
    show (DBAbs t) = join ["(λ", show t, ")"]
    show (DBApp t1 t2) = join ["(", show t1, " ", show t2, ")"]

toDeBruijn :: Term -> DBTerm
toDeBruijn = toDeBruijnRec [] where
    toDeBruijnRec :: [String] -> Term -> DBTerm
    toDeBruijnRec vars (Var x) = case elemIndex x vars of
        Just n -> DBVar (n+1)
        Nothing -> DBConst x
    toDeBruijnRec vars (Abs x t) = DBAbs (toDeBruijnRec (x:vars) t)
    toDeBruijnRec vars (App t1 t2) = DBApp (toDeBruijnRec vars t1) (toDeBruijnRec vars t2)
    toDeBruijnRec _ Empty = error "Empty detected"

eval :: Term -> DBTerm
eval t = dbEval (toDeBruijn t) where
    dbEval :: DBTerm -> DBTerm
    dbEval (DBApp (DBAbs t) t1) = let t1' = dbEval t1 in dbEval (sub t t1' 1)
    dbEval (DBApp t1 t2) = case dbEval t1 of
        a@(DBAbs t) -> dbEval (DBApp a t2)
        t -> DBApp t (dbEval t2)
    dbEval (DBAbs t) = DBAbs (dbEval t)
    dbEval t = t

sub :: DBTerm -> DBTerm -> Int -> DBTerm
sub (DBVar n) t n'
    | n == n' = t
    | otherwise = DBVar n
sub c@(DBConst _) _ _ = c
sub (DBAbs t1) t2 n = DBAbs (sub t1 t2 (n+1))
sub (DBApp t1 t2) t n = DBApp (sub t1 t n) (sub t2 t n)