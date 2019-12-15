{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Tokens (calc, VarList, Calculation, runCalculation)
import Data.Map (empty)
import Lexer (lexer)
import Useful.Dictionary ((#+))
import Complex (pretty, Complex)
import System.Console.Haskeline (getInputLine, defaultSettings)
import System.Console.Haskeline.IO (InputState, initializeInput
          , cancelInput, queryInput)
import Control.Exception (bracket)
import Control.Monad.State

{-mainLoop:: InputState -> VarList -> IO ()
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
          (\is -> mainLoop is empty)-}

calculation :: Calculation ()
calculation = do
  line <- liftIO getLine
  x <- calc . lexer $ line
  liftIO $ putStrLn . pretty $ x

{-main :: IO ()
main = evalStateT calculations empty
  where calculations = sequence_ $ repeat calculation-}

main :: IO ()
main = runCalculation calculations empty defaultSettings
  where calculations = sequence_ $ repeat calculation
