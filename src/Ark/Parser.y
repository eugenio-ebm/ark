{
module Ark.Parser ( parse ) where

import Ark.Lexer ( Token(..), lexTokens )
import Ark.Surface ( Expr(..), BinOp(..) )
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
    'let'	{ TokenLet }
    'in'	{ TokenIn }
    '='		{ TokenEq }
    '+'		{ TokenPlus }
    '-'		{ TokenMinus }
    '('		{ TokenLP }
    ')'		{ TokenRP }
    id		{ TokenId $$ }
    int		{ TokenInt $$ }
%%

Expr :: { Expr }
     	: Expr1					{ $1 }

Expr1 :: { Expr }
      	 : 'let' id '=' Expr2 'in' Expr1	{ Let $2 $4 $6 }
	 | Expr2				{ $1 }

Expr2 :: { Expr }
      	 : Binop				{ $1 }
	 | Expr3	  			{ $1 }

Binop :: { Expr }
      	 : Binop1				{ $1 }

Binop1 :: { Expr }
          : Binop1 '+' Expr3			{ BinOp Add $1 $3 }
          | Binop1 '-' Expr3			{ BinOp Sub $1 $3 }
	  | Expr3 '+' Expr3			{ BinOp Add $1 $3 }
	  | Expr3 '-' Expr3			{ BinOp Sub $1 $3 }

Expr3 :: { Expr }
      	 : id					{ Var $1 }
	 | int					{ Int $1 }
      	 | '(' '-' int ')'			{ Int (-$3) }
	 | '(' Expr ')'				{ $2 }

{
parseError :: [Token] -> a
parseError ts = error $ "Error at tokens: " ++ show ts

parse :: String -> Expr
parse = parseExpr . lexTokens
}
