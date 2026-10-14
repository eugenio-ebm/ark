{
module Ark.Surface.Parser ( parse ) where

import Ark.Surface.Lexer ( Token(..), lexTokens )
import Ark.Surface.Expr ( Expr(..), BinOp(..) )
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    '+'		{ TokenPlus }
    '-'		{ TokenMinus }
    '('		{ TokenLP }
    ')'		{ TokenRP }
    int		{ TokenInt $$ }
%%

Expr :: { Expr }
     	: Expr1				{ $1 }

Expr1 :: { Expr }
      	 : Binop			{ $1 }
	 | Expr2	  		{ $1 }

Binop :: { Expr }
      	 : Binop1			{ $1 }

Binop1 :: { Expr }
          : Binop1 '+' Binop2		{ BinOp Add $1 $3 }
          | Binop1 '-' Binop2		{ BinOp Sub $1 $3 }
	  | Binop2     			{ $1 }

Binop2 :: { Expr }
       	  : Expr2			{ $1 }

Expr2 :: { Expr }
      	 : int				{ Int $1 }
      	 | '(' '-' int ')'		{ Int (-$3) }
	 | '(' Expr ')'			{ $2 }

{
parseError :: [Token] -> a
parseError ts = error $ "Error at tokens: " ++ show ts

parse :: String -> Expr
parse = parseExpr . lexTokens
}
