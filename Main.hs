module Main where

import Tokens
import Data.Map(empty)
import Lexer
import Eval
import Control.Monad
import Useful.Dictionary
import Debug.Trace

mainLoop:: VarList -> IO ()
mainLoop list = do
  line <- getLine 
  let exp = calc . lexer $ line
  case exp of
    Let var exp -> do
      let list' = list #+ (var, exp)
      print $ eval exp list'
      mainLoop list'
    _ -> do
      print $ eval exp list
      mainLoop list

main :: IO ()
main = mainLoop empty

{-getLine :: IO String
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
error :: String -> a-}
