{
module AlexScanner
where
}

%wrapper "basic"
$digit = [0-9]
$letter = [a-zA-Z]


tokens :-
    \n          { \s -> NewLine }
    let         { \s -> Let }
    \=          { \s -> Eq }
    $letter+    { \s -> Term s }
    \.          { \s -> Dot }
    \λ          { \s -> Lambda }
    \(          { \s -> OpenPar }
    \)          { \s -> ClosedPar }
    $white      ;

{
    data Token = NewLine
        | Let
        | Eq
        | Term String
        | Dot
        | Lambda
        | OpenPar
        | ClosedPar
    deriving (Eq, Show)
}