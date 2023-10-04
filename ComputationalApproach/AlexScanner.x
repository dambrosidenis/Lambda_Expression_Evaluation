{
module AlexScanner
where
}

%wrapper "basic"
$letter = [a-zA-Z]


tokens :-
    \n          { \s -> NewLine }
    $letter+    { \s -> Term s }
    \.          { \s -> Dot }
    \λ          { \s -> Lambda }
    \(          { \s -> OpenPar }
    \)          { \s -> ClosedPar }
    $white      ;

{
    data Token = NewLine
        | Term String
        | Dot
        | Lambda
        | OpenPar
        | ClosedPar
    deriving (Eq, Show)
}