module Main where

import Data.Map(empty)
import Control.Monad
import Useful.Dictionary
import Tokens
import Lexer
import Eval
import Complex
import LineEditing
import UI.NCurses
import LineEditing

evaluator :: CurrentState -> ScreenSize -> Window -> Curses ()
evaluator ("quit()":_, _) _ _ = return ()
evaluator (line:xs, varlist) wSize w = do
  let exp = calc $ lexer line
  case exp of 
    Let var exp -> do
      let varlist' = varlist #+ (var, exp)
      --showInput w ((pretty $ eval exp varlist') ++ "\n")
      strings <- buildLine  0 ("":(line:xs), varlist') wSize w
      evaluator strings wSize w
    _ -> do
      --showInput w ((pretty $ eval exp varlist) ++ "\n")
      strings <- buildLine  0 ("":(line:xs), varlist) wSize w
      evaluator strings wSize w

main :: IO ()
main = runCurses $ do
          setEcho False
          w <- defaultWindow
          wSize <- screenSize
          firstStr <- buildLine 0 ([""],empty) wSize w
          evaluator firstStr wSize w
          closeWindow w
