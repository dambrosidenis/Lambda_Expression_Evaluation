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
    LET     { Let }
    '='     { Eq }
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
        | exp NL                        { $1 }
        | LET VAR '=' exp NL            { Def $2 $4 }


exp     : VAR                           { Var $1 }
        | '(' exp ')'                   { $2 }
        | 'λ' VAR '.' exp               { Abs $2 $4 }
        | exp exp                       { App $1 $2 }


{

parseError :: [Token] -> a
parseError e = error (show e ++ "Errore durante il parsing")

main :: IO ()
main = do
    s <- readFile "prova.txt"
    let tokens = alexScanTokens s ++ [ NewLine ]
    let parsedTerms = filter (/= Empty) (reverse (parse tokens))
    --let evaluatedTerms = map eval parsedTerms
    let (environment, evaluatedTerms) = foldl (\ (env, evaluated) newTerm -> (updateEnv env (eval env newTerm), evaluated ++ [(eval env newTerm)]) ) ([],[]) parsedTerms
    let evaluatedTermsNoDef = map convertDefs evaluatedTerms
    mapM_ (\(parsed, eval) -> putStrLn (getPrintable parsed ++ " = " ++ getPrintable eval)) (zip parsedTerms evaluatedTermsNoDef)
    --print environment
}