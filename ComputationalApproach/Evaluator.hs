{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( (\\), elemIndex )
import AlexScanner (Token(Term))

data SyntaxTerm = SyntaxVar String
    | SyntaxAbs String SyntaxTerm
    | SyntaxApp SyntaxTerm SyntaxTerm
    | Empty
    deriving (Eq, Show)

data Term = Var String | Abs (Term -> Term) | App Term Term

instance Show Term where
    show :: Term -> String
    show (Var x) = "[VAR]" ++ x
    show (Abs f) = "[ABS]"
    show (App t1 t2) = "[APP] (" ++ show t1 ++ " " ++ show t2 ++ ")"

eval :: SyntaxTerm -> Term
eval = evalRec [] [] where
    evalRec :: [Term] -> [String] -> SyntaxTerm -> Term
    evalRec env vars (SyntaxAbs v t) = Abs (\ x -> evalRec (env ++ [x]) (vars ++ [v]) t)
    evalRec env vars (SyntaxVar x) = case elemIndex x vars of
        Just n -> env !! n
        Nothing -> Var x
    evalRec env vars (SyntaxApp t1 t2) = let t2' = evalRec env vars t2 in case evalRec env vars t1 of
        Abs f -> f t2'
        v@(Var _) -> App v t2'
        a@(App _ _) -> App a t2'
    evalRec _ _ Empty = error "Empty detected"

toLambda :: SyntaxTerm -> Term
toLambda = toLambdaRec [] where
    toLambdaRec :: [String] -> SyntaxTerm -> Term
    toLambdaRec vars (SyntaxVar x) = case elemIndex x vars of
        Just n -> nthProjection n (length vars)
        Nothing -> naryConstant (length vars) x
    toLambdaRec vars (SyntaxAbs x t) = toLambdaRec (vars ++ [x]) t
    toLambdaRec _ Empty = error "Empty detected"
    toLambdaRec vars (SyntaxApp t1 t2) = let t2' = toLambdaRec vars t2 in case apply (toLambdaRec vars t1) t2' of
        Abs f -> f t2'
        v@(Var x) -> App v t2'
        a@(App _ _) -> App a t2'

apply :: Term -> Term -> Term
apply (Abs f) t = f t
apply v@(Var x) t = App v t
apply a@(App t1 t2) t = App a t

nthProjection :: Int -> Int -> Term
nthProjection n arity = nthProjectionRec n arity [] where
    nthProjectionRec :: Int -> Int -> [Term] -> Term
    nthProjectionRec n 0 _ = error "nth-projection: cannot build a 0-ary abstraction"
    nthProjectionRec n 1 values = Abs (\x -> (values++[x]) !! n)
    nthProjectionRec n i values = Abs (\x -> nthProjectionRec n (i-1) (values++[x]))

naryConstant :: Int -> String -> Term
naryConstant 0 value = Var value 
naryConstant 1 value = Abs (const (Var value))
naryConstant i value = Abs (const (naryConstant (i-1) value))