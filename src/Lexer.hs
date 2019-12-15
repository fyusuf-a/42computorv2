module Lexer where

import Tokens
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer ('i':cs) = TokenI : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexVar (c:cs)
  | isDigit c = lexNum (c:cs)
  | otherwise = error $ "Unexpected character " ++ show c

lexNum cs = TokenRatio (read $ num ++ " % 1") : lexer rest
  where (num, rest) = span isDigit cs

lexVar cs =
  let (var, rest) = span isAlpha cs
  in TokenVar var : lexer rest
