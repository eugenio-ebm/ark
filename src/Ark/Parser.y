{
module Ark.Parser ( parseExpr ) where

import Ark.Lexer ( Token(..) )
import Ark.Expr
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
          : Binop1 '+' Binop2		{ Add $1 $3 }
          | Binop1 '-' Binop2		{ Sub $1 $3 }
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
}