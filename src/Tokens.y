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

Exp : var '=' Exp1           { Let $1 $3 }
    | Exp1                   { Exp1 $1 }

Exp1 : Exp1 '+' Term         { Plus $1 $3 }
     | Exp1 '-' Term         { Minus $1 $3 }
     | Term                  { Term $1 }

Term : Term '*' Factor       { Times $1 $3 }
     | Term '/' Factor       { Div $1 $3 }
     | Factor                { Factor $1 }

Factor : var                   { Var $1 }
       | int                   { Int $1 }
       | '(' Exp1 ')'          { Brack $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
  = Let String Exp1
  | Exp1 Exp1
  deriving Show

data Exp1
  = Plus Exp1 Term
  | Minus Exp1 Term
  | Term Term
  deriving Show

data Term
  = Times Term Factor
  | Div Term Factor
  | Factor Factor
  deriving Show

data Factor
  = Int Int
  | Var String
  | Brack Exp1
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
