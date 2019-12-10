module Eval where

import Tokens
import Data.Map
import Useful.Dictionary

{-instance Evaluable Exp where
  eval (Exp e) l = eval e l
  eval (Let s e) l = eval e l

instance Evaluable Exp where
  eval (t1 `Plus` t2) l = eval t1 l + eval t2 l
  eval (t1 `Minus` t2) l = eval t1 l - eval t2 l
  eval (Term t) l = eval t l

instance Evaluable Term where
  eval (t1 `Times` t2) l = eval t1 l * eval t2 l
  eval (t1 `Div` t2) l = eval t1 l `div` eval t2 l
  eval (Factor t) l = eval t l

instance Evaluable Factor where
  eval (Int i) _ = i
  eval (Brack t) l = eval t l
  eval (Var v) list =
    case v #! list of
      Nothing -> error $ "Could not find value " ++ v ++ " in memory"
      Just exp -> eval exp list-}

type VarList = Map String Exp

class Evaluable a where
  eval :: a -> VarList -> Int

instance Evaluable Exp where
  eval (t1 `Plus` t2) l = eval t1 l + eval t2 l
  eval (t1 `Minus` t2) l = eval t1 l - eval t2 l
  eval (t1 `Times` t2) l = eval t1 l * eval t2 l
  eval (t1 `Div` t2) l = eval t1 l `div` eval t2 l
  eval (Int i) _ = i
  eval (Exp e) l = eval e l
  eval (Var v) list =
    case v #! list of
      Nothing -> error $ "Could not find value " ++ v ++ " in memory"
      Just exp -> eval exp list
  eval (Let s e) l = error "Cannot evaluate assignment expression"

