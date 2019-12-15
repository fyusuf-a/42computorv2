module Main where

import Tokens (calc, Exp(..))
import Data.Map (empty)
import Lexer (lexer)
import Eval (eval, VarList)
import Useful.Dictionary ((#+))
import Complex (pretty)
import System.Console.Haskeline (getInputLine, defaultSettings)
import System.Console.Haskeline.IO (InputState, initializeInput
          , cancelInput, queryInput)
import Control.Exception (bracket)

mainLoop:: InputState -> VarList -> IO ()
mainLoop is list = do
  input <- queryInput is (getInputLine "") 
  case input of
    Nothing -> mainLoop is list
    Just "quit" -> return ()
    Just line -> do
      let exp = calc . lexer $ line
      case exp of
        Let var exp -> do
          let list' = list #+ (var, exp)
          {-putStrLn . show $ exp-}
          putStrLn . pretty $ eval exp list'
          mainLoop is list'
        _ -> do
          {-putStrLn . show $ exp-}
          putStrLn . pretty $ eval exp list
          mainLoop is list

main :: IO ()
main = bracket
          (initializeInput defaultSettings)
          cancelInput
          (\is -> mainLoop is empty)
