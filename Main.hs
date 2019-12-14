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
                            updateWindow w $ moveBackOneChar screenSize
                            showInput w " "
                            updateWindow w $ moveBackOneChar screenSize
                            render

eraseWholeString :: Window -> ScreenSize -> String -> Curses ()
eraseWholeString w screenSize (x:xs) = do 
                                      eraseLastChar w screenSize
                                      eraseWholeString w screenSize $ init (x:xs)
eraseWholeString w screenSize [] = return ()

evaluator :: CurrentState -> ScreenSize -> Window -> Curses ()
evaluator ("quit()":_, _) _ _ = return ()
evaluator (line:xs, varlist) screenSize w = do
  let exp = calc $ lexer line
  case exp of 
    Let var exp -> do
      let varlist' = varlist #+ (var, exp)
      showInput w ((pretty $ eval exp varlist') ++ "\n")
      buildLine  0 ("":(line:xs), varlist') screenSize w
    _ -> do
      showInput w ((pretty $ eval exp varlist) ++ "\n")
      buildLine  0 ("":(line:xs), varlist) screenSize w

increaseListBrowser :: Int -> [String] -> Int
increaseListBrowser num strings = if num >= (length strings - 1)
                                        then (length strings) - 1
                                        else (num + 1)

replaceModifiedString :: Int -> String -> [String] -> [String]
replaceModifiedString num firstStr (x:xs) = if num /= 0
                                              then (firstStr:(x:xs))
                                              else (firstStr:(xs))

buildLine  :: Int -> CurrentState -> ScreenSize -> Window -> Curses ()
buildLine  num (strings,variables) screenSize w = do
  let str = strings !! num
  ev <- getEvent w Nothing
  case ev of
    Just (EventCharacter c) -> case c of
                                 '\n' -> do
                                    showInput w [c]
                                    case str of
                                      "" -> buildLine  0 (strings,variables) screenSize w
                                      _ -> evaluator ((str:strings), variables) screenSize w
                                 c -> do 
                                    showInput w [c]
                                    buildLine  0 ((replaceModifiedString num (str ++ [c]) strings),variables) screenSize w
    Just (EventSpecialKey k) -> case k of 
                                  KeyUpArrow -> do
                                               let newNum = increaseListBrowser num strings
                                               eraseWholeString w screenSize str
                                               showInput w $ strings !! newNum
                                               buildLine newNum (strings,variables) screenSize w
                                  KeyDownArrow -> do
                                               let newNum = (\x -> if x /= 0 then x - 1 else x) num
                                               eraseWholeString w screenSize str
                                               showInput w $ strings !! newNum
                                               buildLine newNum (strings,variables) screenSize w
                                  KeyBackspace ->
                                    case str of
                                      [] -> buildLine  0 ("":strings,variables) screenSize w
                                      str -> do
                                        eraseLastChar w screenSize
                                        buildLine  0 ((tail str):strings,variables) screenSize w
                                  _ -> buildLine  0 (strings,variables) screenSize w
  return ()

main :: IO ()
main = runCurses $ do
          setEcho False
          w <- defaultWindow
          wSize <- screenSize
          buildLine  0 ([""],empty) wSize w 
          closeWindow w
