module Eval where

import Tokens
import Data.Map
import Useful.Dictionary

type VarList = Map String Exp

eval :: Exp -> VarList -> Int
eval (t1 `Plus` t2) l = eval t1 l + eval t2 l
eval (t1 `Minus` t2) l = eval t1 l - eval t2 l
eval (t1 `Times` t2) l = eval t1 l * eval t2 l
eval (t1 `Div` t2) l = eval t1 l `div` eval t2 l
eval (Int i) _ = i
eval (Exp e) l = eval e l
eval (Var v) list =
  case v #! list of
    Nothing -> error $ "Could not find value " ++ v ++ " in memory."
    Just exp -> eval exp list
eval (Let s e) l = error "Cannot evaluate assignment expression"
