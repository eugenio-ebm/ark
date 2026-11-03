{
module Lexer ( Token(..), Lexer.lex ) where
}

%wrapper "basic"

$digit = 0-9

$alpha = [a-zA-Z]

$id = [a-zA-Z0-9']

tokens :-
    $white+		    ;
    let			    { \_ -> TokenLet }
    in	       	      	    { \_ -> TokenIn }
    \=	       	      	    { \_ -> TokenEq }

    \+		      	    { \_ -> TokenPlus }
    \-	       	    	    { \_ -> TokenMinus }
    \*	       	    	    { \_ -> TokenStar }

    \<		      	    { \_ -> TokenLt }

    \++	       	      	    { \_ -> TokenPlusPlus }

    \(		      	    { \_ -> TokenLP }
    \)	       	    	    { \_ -> TokenRP }

    \\		      	    { \_ -> TokenBackslash }
    \->		    	    { \_ -> TokenArr }

    match	      	    { \_ -> TokenMatch }
    \|		      	    { \_ -> TokenBar }
    otherwise		    { \_ -> TokenOtherwise }
    end		      	    { \_ -> TokenEnd }

    $alpha $id*    	    { \s -> TokenId s }
    \' ($alpha|$digit) \'   { \s -> TokenChar (read s) }
    \" [^\"]* \"       	    { \s -> TokenStr (read s) }
    $digit+    	    	    { \s -> TokenInt (read s) }

{
data Token
  = TokenLet
  | TokenIn
  | TokenEq

  | TokenPlus
  | TokenMinus
  | TokenStar

  | TokenLt

  | TokenPlusPlus

  | TokenLP
  | TokenRP

  | TokenBackslash
  | TokenArr

  | TokenMatch
  | TokenBar
  | TokenOtherwise
  | TokenEnd

  | TokenId String
  | TokenChar Char
  | TokenStr String
  | TokenInt Int
  deriving (Eq, Show)

lex :: String -> [Token]
lex = alexScanTokens
}
