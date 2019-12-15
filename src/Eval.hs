module Eval where

import Tokens
import Useful.Dictionary
import Complex
import Data.Map

type VarList = Map String Exp

eval :: Exp -> VarList -> Complex
eval (t1 `Plus` t2) l = eval t1 l + eval t2 l
eval (t1 `Minus` t2) l = eval t1 l - eval t2 l
eval (t1 `Times` t2) l = eval t1 l * eval t2 l
eval (t1 `Div` t2) l = eval t1 l / eval t2 l
eval (Number r) _ = Complex r 0
eval I _ = Complex 0 1
eval (Var v) list =
  case v #! list of
    Nothing -> error $ "Could not find value " ++ v ++ " in memory."
    Just exp -> eval exp list
eval (Let s e) l = error "Cannot evaluate assignment expression"
