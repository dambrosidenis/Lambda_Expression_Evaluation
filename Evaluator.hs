{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( (\\), elemIndex, intercalate )
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

toLambda :: SyntaxTerm -> Term
toLambda = toLambdaRec [] where
    toLambdaRec :: [String] -> SyntaxTerm -> Term
    toLambdaRec vars (SyntaxVar x) = case elemIndex x vars of
        Just n -> nthProjection n (length vars)
        Nothing -> naryConstant (length vars) x
    toLambdaRec vars (SyntaxAbs x t) = toLambdaRec (vars ++ [x]) t
    toLambdaRec _ Empty = error "Empty detected"
    toLambdaRec vars (SyntaxApp t1 t2) = case t1 of
        t@(SyntaxAbs _ _) -> let (Abs f) = toLambdaRec vars t in f (toLambdaRec vars t2) -- OK
        t@(SyntaxVar x) -> case elemIndex x vars of
            Just n -> nthApplication n (toLambdaRec vars t2) (length vars) -- DEVI PASSARE L'ALBERO SINTATTICO AL POSTO DI n!!
            Nothing -> App (Var x) (toLambdaRec vars t2) -- OK
        t@(SyntaxApp t2 t3) -> toLambdaRec vars t
        Empty -> error "Empty detected"

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

nthApplication :: Int -> Term -> Int -> Term
nthApplication n term arity = nthApplicationRec n term arity [] where
    nthApplicationRec :: Int -> Term -> Int -> [Term] -> Term
    nthApplicationRec n term 1 values = Abs (\ x -> case (values++[x]) !! n of
        Abs f -> f term
        t@(Var v) -> App t term
        _ -> error "Illegal application")
    nthApplicationRec n term i values = Abs (\ x -> nthApplicationRec n term (i-1) (values ++ [x]))