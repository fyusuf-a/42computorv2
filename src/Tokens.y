{
module Tokens where

import Complex (Complex(..), (^^^))
import Control.Monad.State (get, put)
import Useful ((#!), (#+))
import App
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
  '^'   { TokenPower }
  '('   { TokenOB }
  ')'   { TokenCB }
  '?'   { TokenQuestionMark }

%left '='
%left '+' '-'
%left '*' '/'
%left NEG
%left '^'
%%

Exp :: { StateT VarList Complex }
    | var '=' NumExp { Let $1 $3 }
    | NumExp '=' '?' { NumExp $1 }
    | var            { Var $1 }

NumExp :: { Complex }
       | NumExp '+' NumExp    { $1 $3 }
       | NumExp '-' NumExp    { $1 $3 }
       | NumExp '*' NumExp    { Times $1 $3 }
       | NumExp '/' NumExp    { Div $1 $3 }
       | NumExp '^' NumExp    { Power $1 $3 }
       | '-' NumExp %prec NEG { Negate $2 }
       | '(' NumExp ')'       { $2 }
       | i                    { I }
       | num                  { Number $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
  = Let String NumExp
  | Exp NumExp
  | Var String
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
  | TokenPower
  | TokenOB
  | TokenCB
  | TokenQuestionMark
  deriving Show
}
