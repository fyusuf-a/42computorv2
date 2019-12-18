{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Tokens (calc)
import App (Calculation, runCalculation)
import Data.Map (empty)
import Lexer (lexer)
import Complex (pretty)
import System.Console.Haskeline (getInputLine, defaultSettings)
import Control.Monad.State.Strict (liftIO, lift)
import Control.Exception (catch)

calculation :: Calculation ()
calculation = do
  interaction <- getInputLine "% "
  case interaction of
    Nothing -> return ()
    Just line -> do
      x <- lift $ calc . lexer $ line
      liftIO . putStrLn . pretty $ x

main :: IO ()
main = runCalculation calculations empty defaultSettings
  where calculations = sequence_ . repeat $
              catch calculation $ \(e::IOError) -> putStrLn "yo"
