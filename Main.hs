module Main where

import Tokens
import Data.Map(empty)
import Lexer
import Eval
import Control.Monad
main :: IO ()
main = do
  line <- getLine 
  case calc . lexer $ line of
    Let _ _ -> error "To be implemented"
    x -> print $ eval x empty
  main



{-getLine :: IO String
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
error :: String -> a-}
