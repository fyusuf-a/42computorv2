{
module Tokens where

import Complex
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  'i'   { TokenI }
  num   { TokenRatio $$ }
  var   { TokenVar $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenTimes }
  '/'   { TokenDiv }
  '('   { TokenOB }
  ')'   { TokenCB }

%%

Exp : var '=' Exp1           { Let $1 $3 }
    | Exp1                   { Exp $1 }

Exp1 : Exp1 '+' Term         { Plus $1 $3 }
     | Exp1 '-' Term         { Minus $1 $3 }
     | Term                  { Exp $1 }

Term : Term '*' Factor       { Times $1 $3 }
     | Term '/' Factor       { Div $1 $3 }
     | Factor                { Exp $1 }

Factor : 'i'                 { I }
       | var                 { Var $1 }
       | num                 { Number $1 }
       | '(' Exp ')'         { Exp $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
  = Let String Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Number Rational
  | Exp Exp
  | Var String
  | I
  deriving Show

data Token
  = TokenVar String
  | TokenI
  | TokenRatio Rational
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB
  deriving Show
}
