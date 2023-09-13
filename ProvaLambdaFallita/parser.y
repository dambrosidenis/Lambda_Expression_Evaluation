{
module Main(main) where
import AlexScanner
import Evaluator
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

%left 'λ' '.'
%right VAR

%%

start   : line                          { [$1] }
        | start line                    { $2 : $1 }

line    : NL                            { Empty }
        | exp NL                        { eval $1 }


exp     : VAR                           { Variable (toIndex $1) createEnv }
        | '(' exp ')'                   { $2 }
        | 'λ' VAR '.' exp               { Abstraction (createLambda (Variable (toIndex $2) createEnv) $4)
                                                (updateEnv $4 $2 (createLambda (Variable (toIndex $2) createEnv) $4))
                                                (toIndex $2) }
        | exp exp                       { apply $1 $2 }

{

parseError :: [Token] -> a
parseError e = error (show e ++ "Errore durante il parsing")

main :: IO ()
main = do
    s <- readFile "prova.txt"
    let tokens = alexScanTokens s ++ [ NewLine ]
    let parsedTerms = filter notEmpty (reverse (parse tokens))
    mapM_ print parsedTerms
}