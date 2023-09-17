module Evaluator where

import Data.List ( (\\), elemIndex )
 
data Term =
    Var String
    | Abs String Term
    | App Term Term
    | Def String Term
    | Empty
    deriving (Eq, Show)

eval :: [(String, Term)] -> Term -> Term
eval env t = case t of
        (Def x t) -> Def x (evalRec (substEnv t))
        (Abs x t) -> if notElem x names then Abs x (evalRec (substEnv t)) else error "Name already used"
        t -> evalRec (substEnv t)
        where
            substEnv :: Term -> Term
            substEnv = substEnv' names values
            names :: [String]
            names = map fst env
            values :: [Term]
            values = map snd env
            substEnv' :: [String] -> [Term] -> Term -> Term
            substEnv' names values (Var x) = case elemIndex x names of
                (Just index) -> values !! index
                Nothing -> Var x
            substEnv' names values (Abs x t) = Abs x (substEnv' names values t)
            substEnv' names values (App t1 t2) = App (substEnv' names values t1) (substEnv' names values t2)
            substEnv' names values (Def x t) = Def x (substEnv' names values t)
            substEnv' _ _ t = t
            
evalRec :: Term -> Term
evalRec (App (Abs x t12) v2@(Abs _ _))  = subst x t12 v2 -- E-AppAbs
evalRec (App (Abs x t12) (Var y))       = subst x t12 (Var y)
evalRec (App v1@(Abs _ _) t2)           = let t2' = evalRec t2 in App v1 t2' -- E-App2
evalRec (App t1 t2)                     = let t1' = evalRec t1 in App t1' t2 -- E-App1
evalRec (Abs x t)                       = Abs x (evalRec t)
evalRec t = t
--evalRec _ = error "No rule applies"

updateEnv :: [(String, Term)] -> Term -> [(String, Term)]
updateEnv environment (Def v t) = environment ++ [(v, t)]
updateEnv environment _ = environment

subst :: String -> Term -> Term -> Term
subst x (Var v) newVal
    | x == v    = newVal
    | otherwise = Var v
subst x (Abs y t1) newVal
    | x == y                                  = Abs y t1
    | x /= y && (y `notElem` freeVars newVal) = Abs y (subst x t1 newVal)
    | otherwise                               = error $ "Cannot substitute '" ++ show x ++ "' in term '" ++ show (Abs y t1) ++ "'"
subst x (App t1 t2) newVal = App (subst x t1 newVal) (subst x t2 newVal)
subst _ _ _ = error "Illegal substitution"

freeVars :: Term -> [String]
freeVars (Var x) = [x]
freeVars (Abs x t1) = removeDuplicates (freeVars t1) \\ [x] where
    removeDuplicates :: [String] -> [String]
    removeDuplicates [] = []
    removeDuplicates (x:xs)
        | elem x xs     = removeDuplicates xs
        | otherwise     = x : removeDuplicates xs
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Def v t) = freeVars t
freeVars _ = error "Empty detected"

getPrintable :: Term -> String
getPrintable (Var x) = x
getPrintable (Abs x term) = "(λ" ++ x ++ "." ++ getPrintable term ++ ")"
getPrintable (App x y) = "(" ++ getPrintable x ++ ") (" ++ getPrintable y ++ ")"
getPrintable (Def x t) = "let " ++ x ++ " = " ++ getPrintable t
getPrintable _ = error "Empty detected"

convertDefs :: Term -> Term
convertDefs (Def x t) = t
convertDefs t = t