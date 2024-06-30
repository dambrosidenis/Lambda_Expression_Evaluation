{
module Main(main) where
import Scanner
import System.Environment
import LambdaTerm
import qualified AlgebraicAlphaConversion as AAC
import qualified AlgebraicDeBrujin as ADB
import qualified Computational as COM
}

%name parse
%tokentype {Token}
%error {parseError}

%token
    NL      { NewLine }
    VAR     { Term $$ }
    '.'     { Dot }
    'λ'     { Lambda }
    '('     { OpenPar }
    ')'     { ClosedPar }

%left 'λ' '.' VAR

%%

start   : line          { [$1] }
        | start line    { $2 : $1 }

line    : NL            { Empty }
        | exp NL        { $1 }

exp     : VAR                       { Var $1 }
        | '(' exp ')'               { $2 }
        | 'λ' VAR '.' exp           { Abs $2 $4 }
        | exp exp                   { App $1 $2 }


{

parseError :: [Token] -> a
parseError e = error (show e ++ "Errore durante il parsing")

main :: IO ()
main = do

    -- Read the command line arguments
    args <- getArgs

    if length args /= 1
        then putStrLn "Usage: ./parser <filepath>"
        else do
            -- Read the filepath
            let filePath = head args

            -- Read the input file
            s <- readFile filePath

            -- Split the input sequence into a sequence of tokens
            let tokens = alexScanTokens s ++ [ NewLine ]

            -- Parse the sequence of tokens into a series of expressions
            let parsedTerms = filter (/= Empty) (reverse (parse tokens))
            putStr "Original terms\n"
            mapM_ (print) parsedTerms
            putStr "\n"

            -- Use the Alpha Conversion method for evaluation
            putStr "Algebraic Alpha Conversion method\n"
            let evaluatedAACTerms = map AAC.eval parsedTerms
            mapM_ (print) evaluatedAACTerms
            putStr "\n"

            -- Use the De Brujin method for evaluation
            putStr "Algebraic De Brujin method\n"
            let evaluatedADBTerms = map ADB.eval parsedTerms
            mapM_ (print) evaluatedADBTerms
            putStr "\n"

            -- Use the Computational method for evaluation
            putStr "Computational method\n"
            let evaluatedCOMTerms = map COM.eval parsedTerms
            mapM_ (print) evaluatedCOMTerms

}