module Main where

import Tokens
import Lexer
import Eval
import Control.Monad

main :: IO ()
main = forever $
  getLine >>= print . calc . lexer
