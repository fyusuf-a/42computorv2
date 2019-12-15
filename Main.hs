module Main where

import Tokens
import Data.Map(empty)
import Lexer
import Eval
import Control.Monad
import Useful.Dictionary
import Debug.Trace
import Complex

mainLoop:: VarList -> IO ()
mainLoop list = do
  line <- getLine 
  let exp = calc . lexer $ line
  case exp of
    Let var exp -> do
      let list' = list #+ (var, exp)
      {-putStrLn . show $ exp-}
      putStrLn . pretty $ eval exp list'
      mainLoop list'
    _ -> do
      {-putStrLn . show $ exp-}
      putStrLn . pretty $ eval exp list
      mainLoop list

main :: IO ()
main = mainLoop empty
