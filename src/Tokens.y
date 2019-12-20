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
%monad { StateT VarList IO }

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

Exp :: { Complex }
       : var '=' Exp        {% do ; l <- get ; put $ l #+ ($1, $3) ; return $3 }
       | Exp '+' Exp        { $1 + $3 }
       | Exp '=' '?'        { $1 }
       | Exp '-' Exp        { $1 - $3 }
       | Exp '*' Exp        { $1 * $3 }
       | Exp '/' Exp        { $1 / $3 }
       | Exp '^' Exp        { $1 ^^^ $3 }
       | '-' Exp %prec NEG  { -$2 }
       | '(' Exp ')'        { $2 }
       | Exp i %prec TIMESI { $1 * i }
       | i                  { i }
       | num                { fromRational $1 }
       | var                {% do ; l <- get ; case $1 #! l of ; Nothing -> throw NotInMemory ; Just x -> return x }

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
