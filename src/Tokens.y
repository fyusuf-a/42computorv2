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
%monad { Calculation }

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

%left '='
%left '+' '-'
%left '*' '/'
%left NEG
%left '^'
%%

Exp :: { Complex }
    : var '=' Exp       {% addToState $1 $3 }
    | Exp '+' Exp       { $1 + $3 }
    | Exp '-' Exp       { $1 - $3 }
    | Exp '*' Exp       { $1 * $3 }
    | Exp '/' Exp       { $1 / $3 }
    | Exp '^' Exp       { $1 ^^^ $3 }
    | '-' Exp %prec NEG { - $2 }
    | '(' Exp ')'       { $2 }
    | i                 { Complex 0 1 }
    | var               {% findVar $1 }
    | num               { Complex $1 0 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

addToState :: String -> Complex -> Calculation Complex
addToState var x = do
  l <- get
  put (l #+ (var, x))
  return x

findVar :: String -> Calculation Complex
findVar var = do
  l <- get
  case var #! l of
    Nothing -> error $ "Could not find value " ++ var ++ " in memory."
    Just x -> return x

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
  deriving Show
}
