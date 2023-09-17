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