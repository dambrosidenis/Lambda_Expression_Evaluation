{-# LANGUAGE InstanceSigs #-}
module Evaluator where

import Data.List ( elemIndex )
import Control.Monad ( replicateM )

data LambdaTerm = Abstraction (LambdaTerm -> LambdaTerm) Env Int
    | Variable Int Env
    | Empty

type Env = Int -> Maybe (LambdaTerm -> LambdaTerm)

instance Show LambdaTerm where
    show :: LambdaTerm -> String
    show (Abstraction _ _ n) = "(λ" ++ variablesSet !! n ++ ".ABS)"
    show (Variable n _) = variablesSet !! n
    show Empty = error "Empty detected"

notEmpty :: LambdaTerm -> Bool
notEmpty Empty = False
notEmpty _ = True

createEnv :: Env
createEnv = const Nothing

updateEnv :: LambdaTerm -> String -> (LambdaTerm -> LambdaTerm) -> Env
updateEnv (Abstraction _ env _) var f = updateEnv' (toIndex var) f env
updateEnv (Variable _ env) var f = updateEnv' (toIndex var) f env
updateEnv Empty _ _ = error "Empty detected"

updateEnv' :: Int -> (LambdaTerm -> LambdaTerm) -> Env -> Env
updateEnv' n f env = \ x -> if x == n then Just f else env n

variablesSet :: [String]
variablesSet = concatMap (\k -> replicateM k ['a'..'z']) [1..]

createLambda :: LambdaTerm -> LambdaTerm -> (LambdaTerm -> LambdaTerm)
createLambda (Variable n1 env1) (Variable n2 _)
    | n1 == n2 = (\ x -> x)
    | otherwise = case env1 n2 of
        Just f -> f
        Nothing -> error "Function not in scope"
createLambda (Variable n1 env1) (Abstraction f _ n2) = (\ n1 -> f n1)
createLambda _ _ = error "Invalid syntax"

apply :: LambdaTerm -> LambdaTerm -> LambdaTerm
apply f x = case f of
    (Abstraction g env _) -> g x
    (Variable n env) -> case env n of
        Just func -> func x
        Nothing -> error "Function not in scope"
    Empty -> error "Empty detected"

toIndex :: String -> Int
toIndex n = case elemIndex n variablesSet of
    Just x -> x
    Nothing -> error "Illegal variable"

eval :: LambdaTerm -> LambdaTerm
eval abs@(Abstraction _ env _) = eval' abs env
eval var@(Variable _ env) = eval' var env

eval' :: LambdaTerm -> Env -> LambdaTerm
eval' f x = case f of
    (Abstraction g env _) -> g x
    (Variable n env) -> case env n of
        Just func -> func x
        Nothing -> error "Function not in scope"
    Empty -> error "Empty detected"