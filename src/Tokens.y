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
  int   { TokenInt $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenTimes }
  '/'   { TokenDiv }
  '^'   { TokenPower }
  '('   { TokenOB }
  ')'   { TokenCB }

%left '='
%left '+' '-'
%left '*' '/'
%left NEG
%left '^'
%%

Exp : var '=' Exp       { Let $1 $3 }
    | Exp '+' Exp       { Plus $1 $3 }
    | Exp '-' Exp       { Minus $1 $3 }
    | Exp '*' Exp       { Times $1 $3 }
    | Exp '/' Exp       { Div $1 $3 }
    | Exp '^' Int       { Power $1 $3 }
    | '-' Exp %prec NEG { Negate $2 }
    | '(' Exp ')'       { Exp $2 }
    | i                 { I }
    | var               { Var $1 }
    | num               { Number $1 }

Int : int               { Int $1 }
    | '(' brackInt ')'  { BrackInt $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
  = Let String Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Power Exp Integer
  | Negate Exp
  | Number Rational
  | Exp Exp
  | Var String
  | I
  deriving Show

data Token
  = TokenVar String
  | TokenI
  | TokenRatio Rational
  | TokenInt Integer
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenPower
  | TokenOB
  | TokenCB
  deriving Show
}
