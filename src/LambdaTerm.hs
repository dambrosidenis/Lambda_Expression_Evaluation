module LambdaTerm where

import Data.List ( (\\), elemIndex )
import Control.Monad (replicateM, join)
import Text.Read (readMaybe)

data Term = Var String
    | Abs String Term
    | App Term Term
    | Empty
    deriving (Eq)

instance Show Term where
    show :: Term -> String
    show (Var x) = x
    show (Abs x t) = join ["(λ", x, ".", show t, ")"]
    show (App x y) = join ["(", show x, " ", show y, ")"]
    show _ = error "Empty detected"