{
module Parser ( parse ) where

import Lexer ( Token(..) )
import qualified Lexer

import Syntax ( Expr(..), BinOp(..) )
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
    '*'		{ TokenStar }

    '<'		{ TokenLt }

    '++'	{ TokenPlusPlus }

    '('		{ TokenLP }
    ')'		{ TokenRP }

    'true'	{ TokenTrue }
    'false'	{ TokenFalse }

    '\\'	{ TokenBackslash }
    '->'	{ TokenArr }

    id		{ TokenId $$ }
    str		{ TokenStr $$ }
    int		{ TokenInt $$ }
%%

Expr :: { Expr }
     	: Expr1					{ $1 }

Expr1 :: { Expr }
      	 : 'let' id '=' Expr2 'in' Expr1	{ Let $2 $4 $6 }
	 | Expr2				{ $1 }

Expr2 :: { Expr }
      	 : Binop				{ $1 }

Binop :: { Expr }
      	 : Binop1				{ $1 }

Binop1 :: { Expr }
       	  : Binop2 '<' Binop2			{ BinOp Lt $1 $3 }
	  | Binop1 '++' Binop2			{ BinOp Append $1 $3 }
	  | Binop2     				{ $1 }

Binop2 :: { Expr }
          : Binop2 '+' Binop3			{ BinOp Add $1 $3 }
          | Binop2 '-' Binop3			{ BinOp Sub $1 $3 }
	  | Binop3				{ $1 }

Binop3 :: { Expr }
       	  : Binop3 '*' Binop4			{ BinOp Mul $1 $3 }
	  | Binop4     				{ $1 }

Binop4 :: { Expr }
       	  : Expr3				{ $1 }

Expr3 :: { Expr }
      	 : Expr3 Expr4				{ App $1 $2 }
	 | Expr4				{ $1 }

Expr4 :: { Expr }
      	 : id					{ Var $1 }
	 | str					{ Str $1 }
	 | int					{ Int $1 }
      	 | '(' '-' int ')'			{ Int (-$3) }
	 | 'true'				{ Bool True }
	 | 'false'				{ Bool False }
	 | '\\' id '->' Expr			{ Lam $2 $4 }
	 | '(' Expr ')'				{ $2 }

{
parseError :: [Token] -> a
parseError ts = error $ "Error at tokens: " ++ show ts

parse :: String -> Expr
parse = parseExpr . Lexer.lex
}
