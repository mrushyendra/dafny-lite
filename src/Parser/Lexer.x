{
module Parser.Lexer ( Token (..)
                    , lexProg ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$symb = [\_]

tokens:-
    $white+                             ;

    "if"                                { const TIf }
    "then"                              { const TThen }
    "else"                              { const TElse }
    "end"                               { const TEnd }
    "while"                             { const TWhile }
    "forall"                            { const TForall }
    "exists"                            { const TExists }
    "inv"                               { const TInv }
    "pre"                               { const TPre }
    "post"                              { const TPost }
    "do"                                { const TDo }
    "program"                           { const TProgram }
    "is"                                { const TIs }

    $digit+                             { TokenInt . read }
    $alpha [$alpha $digit $symb ]*      { TokenName }
    [\[ \] \+ \- \* \/ \% ]             { TokenSymb }
    [\( \) ]                            { TokenSymb }
    
    [\= \< \>]                          { TokenSymb }
    \!\=                                { TokenSymb }
    \>\=                                { TokenSymb }
    \<\=                                { TokenSymb }
    \!                                  { TokenSymb }
    
    \|\|                                { TokenSymb }
    \&\&                                { TokenSymb }

    \=\=\>                              { TokenSymb }

    \:\=                                { TokenSymb }
    \,                                  { TokenSymb }
    \;                                  { TokenSymb }

{
data Token = TokenInt Int
           | TokenName String
           | TokenSymb String
           | TIf
           | TThen
           | TElse
           | TEnd
           | TWhile
           | TForall
           | TExists
           | TInv
           | TPre
           | TPost
           | TDo
           | TProgram
           | TIs
           deriving (Eq, Show)

lexProg :: String -> [Token]
lexProg = alexScanTokens

}
