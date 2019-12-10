{
module Tokens where
}


%name calc
%tokentype { Token }
%error { parseError }

%token
  int   { TokenInt $$ }
  var   { TokenVar $$ }
  '='   { TokenEq }
  '+'   { TokenPlus }
  '-'   { TokenMinus }
  '*'   { TokenTimes }
  '/'   { TokenDiv }
  '('   { TokenOB }
  ')'   { TokenCB }

%%

Exp : var '=' Exp           { Let $1 $3 }
    | Exp                   { Exp $1 }

Exp : Exp '+' Term         { Plus $1 $3 }
     | Exp '-' Term         { Minus $1 $3 }
     | Term                  { Exp $1 }

Term : Term '*' Factor       { Times $1 $3 }
     | Term '/' Factor       { Div $1 $3 }
     | Factor                { Exp $1 }

Factor : var                   { Var $1 }
       | int                   { Int $1 }
       | '(' Exp ')'          { Exp $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
  = Let String Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Int Int
  | Exp Exp
  | Var String
  deriving Show

data Token
  = TokenVar String
  | TokenInt Int
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB
  deriving Show
}
