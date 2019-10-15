{
module Parser.Parser (parseProg) where

import Language
import Parser.Lexer
}

%name parse1
%tokentype { Token }
%error { parseError }

%token
    int         { TokenInt $$ }
    name        { TokenName $$ }
    '['         { TokenSymb "[" }
    ']'         { TokenSymb "]" }
    '+'         { TokenSymb "+" }
    '-'         { TokenSymb "-" }
    '*'         { TokenSymb "*" }
    '/'         { TokenSymb "/" }
    '%'         { TokenSymb "%" }
    '('         { TokenSymb "(" }
    ')'         { TokenSymb ")" }
    
    '='         { TokenSymb "=" }
    "!="        { TokenSymb "!=" }
    "<="        { TokenSymb "<=" }
    ">="        { TokenSymb ">=" }
    '<'         { TokenSymb "<" }
    '>'         { TokenSymb ">" }
    '!'         { TokenSymb "!" }
    
    "||"        { TokenSymb "||" }
    "&&"        { TokenSymb "&&" }

    '==>'       { TokenSymb "==>" }

    ":="        { TokenSymb ":=" }
    ','         { TokenSymb "," }
    ';'         { TokenSymb ";" }

    "if"        { TIf }
    "then"      { TThen }
    "else"      { TElse }
    "end"       { TEnd }
    "while"     { TWhile }
    "do"        { TDo }
    "inv"       { TInv }
    "pre"       { TPre }
    "post"      { TPost }
    "program"   { TProgram }
    "is"        { TIs }
    "forall"    { TForall }
    "exists"    { TExists }


%left '+' '-'
%left '*' '/' '%'

%left "||"
%left "&&"
%left '!'

%%

prog :: { Program }
     : "program" name pres posts "is" block "end" { ($2, $3, $4, $6) }

arithExp :: { ArithExp }
         : int { Num $1 }
         | name { Var $1 }
         | '-' arithExp { Sub (Num 0) $2 }
         | name '[' arithExp ']' { Read $1 $3 }
         | arithExp '+' arithExp { Add $1 $3 }
         | arithExp '-' arithExp { Sub $1 $3 }
         | arithExp '*' arithExp { Mul $1 $3 }
         | arithExp '/' arithExp { Div $1 $3 }
         | arithExp '%' arithExp { Mod $1 $3 }
         | '(' arithExp ')'      { Parens $2 }

comp :: { Comparison }
     : arithExp '=' arithExp { Eq $1 $3 }
     | arithExp "!=" arithExp { Neq $1 $3 }
     | arithExp "<=" arithExp { Le $1 $3 }
     | arithExp ">=" arithExp { Ge $1 $3 }
     | arithExp '<' arithExp { Lt $1 $3 }
     | arithExp '>' arithExp { Gt $1 $3 }

boolExp :: { BoolExp }
        : comp { BCmp $1 }
        | '!' boolExp { BNot $2 }
        | boolExp "||" boolExp { BDisj $1 $3 }
        | boolExp "&&" boolExp { BConj $1 $3 }
        | '(' boolExp ')' { BParens $2 }

quantified :: { [Name] }
quantified : name { [$1] }
           | quantified name { $2:$1 }

assn :: { Assertion }
     : comp { AComp $1 }
     | '!' assn { ANot $2 }
     | assn "||" assn { ADisj $1 $3 }
     | assn "&&" assn { AConj $1 $3 }
     | assn '==>' assn { AImplies $1 $3 }
     | "forall" quantified ',' assn { AForall $2 $4 }
     | "exists" quantified ',' assn { AExists $2 $4 }
     | '(' assn ')' { AParens $2 }

invs :: { Inv }
invs : invs_rev { reverse $1 }

invs_rev :: { Inv }
invs_rev : {- empty -} { [] }
         | invs_rev "inv" assn {$3:$1}

pres :: { Pre }
pres : pres_rev { reverse $1 }

pres_rev :: { Pre }
pres_rev : {- empty -} { [] }
         | pres_rev "pre" assn {$3:$1}

posts :: { Post }
posts : posts_rev { reverse $1 }

posts_rev :: { Post }
posts_rev : {- empty -} { [] }
          | posts_rev "post" assn {$3:$1}

stmt :: { Statement }
     : name ":=" arithExp ';' { Assign $1 $3 }
     | name ',' name ":=" arithExp ',' arithExp ';' { ParAssign $1 $3 $5 $7 }
     | name '[' arithExp ']' ":=" arithExp ';' { Write $1 $3 $6 }
     | "if" boolExp "then" block "else" block "end" { If $2 $4 $6 }
     | "if" boolExp "then" block "end" { If $2 $4 [] }
     | "while" boolExp invs "do" block "end" { While $2 $3 $5 }

block :: { Block }
      : block_rev { reverse $1 }

block_rev :: { Block }
          : stmt { [$1] }
          | block_rev stmt {$2:$1}

{

parseProg = parse1 . lexProg

parseError :: [Token] -> a
parseError _ = error "Parse error"

} 
