{
module Tokens where

import Complex
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  i     { TokenI }
  num   { TokenRatio $$ }
  var   { TokenVar $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenTimes }
  '/'   { TokenDiv }
  '('   { TokenOB }
  ')'   { TokenCB }

%left '+' '-'
%left '*' '/'
%%

Exp : var '=' Exp       { Let $1 $3 }
    | Exp '+' Exp       { Plus $1 $3 }
    | Exp '-' Exp       { Minus $1 $3 }
    | Exp '*' Exp       { Times $1 $3 }
    | Exp '/' Exp       { Div $1 $3 }
    | Exp               { Exp $1 }
    | '(' Exp ')'       { Exp $2 }
    | i                 { I }
    | var               { Var $1 }
    | num               { Number $1 }

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
