{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Tokens where

import Complex
import Control.Monad.State (MonadState, StateT, evalStateT, get, put, lift)
import Control.Monad.IO.Class (MonadIO)
import System.Console.Haskeline (InputT, runInputT, Settings, defaultSettings)
import Data.Map (Map)
import Useful ((#!), (#+))
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

type VarList = Map String Complex

newtype Calculation a = Calculation {runCalc :: InputT (StateT VarList IO) a}
  deriving (Functor, Applicative, Monad, MonadState VarList, MonadIO)

instance (MonadState s m) => MonadState s (InputT m) where
  put s = lift $ put s
  get = lift get

runCalculation :: Calculation a -> VarList -> Settings (StateT VarList IO) -> IO a
runCalculation calc st settings = evalStateT st (runInputT settings calc')
  where calc' = runCalc calc

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
