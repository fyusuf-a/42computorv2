{
module Tokens where

import Complex (Complex, (^^^), i)
import Control.Monad.State.Strict (get, put, StateT)
import Useful ((#!), (#+))
import App (VarList)
import Control.Exception (throw, Exception)
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
%left TIMESI
%%

Exp :: { StateT VarList IO Complex }
    : var '=' NumExp { do ; l <- get ; put $ l #+ ($1, $3) ; return $3 }
    | NumExp         { return $1 }
    | NumExp '=' '?' { return $1 }
    | var            { do ; l <- get ; case $1 #! l of ; Nothing -> throw NotInMemory ; Just x -> return x }

NumExp :: { Complex }
       : NumExp '+' NumExp     { $1 + $3 }
       | NumExp '-' NumExp     { $1 - $3 }
       | NumExp '*' NumExp     { $1 * $3 }
       | NumExp '/' NumExp     { $1 / $3 }
       | NumExp '^' NumExp     { $1 ^^^ $3 }
       | '-' NumExp %prec NEG  { -$2 }
       | '(' NumExp ')'        { $2 }
       | NumExp i %prec TIMESI { $1 * i }
       | i                     { i }
       | num                   { fromRational $1 }

{

data CustomException
  = NoParse String
  | NotInMemory

instance Show CustomException where
  show (NoParse "") = "no parse"
  show (NoParse str) = str
  show (NotInMemory) = "variable not in memory"

instance Exception CustomException

parseError :: [Token] -> a
parseError _ = throw $ NoParse ""

data Exp
  = Let String Complex
  | Exp Complex
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
