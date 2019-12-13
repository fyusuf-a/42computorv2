module Main where

import Tokens
import Data.Map(empty)
import Lexer
import Eval
import Control.Monad
import Useful.Dictionary
import Complex
import UI.NCurses

type CurrentState = ([String], VarList)
type ScreenSize = (Integer, Integer)
type CurrentPos = (Integer, Integer)

showInput :: Window -> String -> Curses ()
showInput w string = do
            updateWindow w $ drawString string
            render

moveBackOneChar :: ScreenSize -> Update ()
moveBackOneChar (rowSize,colSize) = do 
                      (posRow, posCol) <- cursorPosition
                      case (posRow, posCol) of
                        (0,0) -> return ()
                        (posRow,0) -> moveCursor (posRow - 1) (colSize - 1)
                        (posRow,posCol) -> moveCursor posRow (posCol - 1)

eraseLastChar :: Window -> ScreenSize -> Curses ()
eraseLastChar w screenSize = do
                            updateWindow w (moveBackOneChar screenSize)
                            showInput w " "
                            updateWindow w (moveBackOneChar screenSize)
                            render

eraseWholeString :: Window -> ScreenSize -> String -> Curses ()
eraseWholeString w screenSize (x:xs) = do 
                                      eraseLastChar w screenSize
                                      eraseWholeString w screenSize (init (x:xs))
eraseWholeString w screenSize [] = return ()

evaluator :: CurrentState -> ScreenSize -> Window -> Curses ()
evaluator ("quit()":_, _) _ _ = return ()
evaluator (line:xs, varlist) screenSize w = do
  let exp = calc $ lexer line
  case exp of 
    Let var exp -> do
      let varlist' = varlist #+ (var, exp)
      showInput w ((pretty $ eval exp varlist') ++ "\n")
      buildLine ("":(line:xs), varlist') screenSize w
    _ -> do
      showInput w ((pretty $ eval exp varlist) ++ "\n")
      buildLine ("":(line:xs), varlist) screenSize w

buildLine :: CurrentState -> ScreenSize -> Window -> Curses ()
buildLine (str:x,variables) screenSize w = do
                          ev <- getEvent w Nothing
                          case ev of
                            Just (EventCharacter c) -> case c of
                                                         '\n' -> do
                                                            showInput w [c]
                                                            evaluator (str:x, variables) screenSize w
                                                         c -> do 
                                                            showInput w [c]
                                                            buildLine ((str ++ [c]):x,variables) screenSize w
                            Just (EventSpecialKey k) -> case k of 
                                                          KeyUpArrow -> do
                                                            eraseWholeString w screenSize str
                                                            showInput w (head x)
                                                            buildLine (x,variables) screenSize w
                                                          KeyBackspace ->
                                                            case str of
                                                              [] -> buildLine ((str):x,variables) screenSize w
                                                              str -> do
                                                                eraseLastChar w screenSize
                                                                buildLine ((tail str):x,variables) screenSize w
                                                          k -> do
                                                            showInput w (show k)
                                                            buildLine (str:x,variables) screenSize w
                            _ -> buildLine (str:x,variables) screenSize w
                          return ()

main :: IO ()
main = runCurses $ do
          setEcho False
          w <- defaultWindow
          wSize <- screenSize
          buildLine ([""],empty) wSize w 
          closeWindow w
