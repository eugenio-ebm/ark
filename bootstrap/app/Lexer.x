{
module Lexer ( Token(..), Lexer.lex ) where
}

%wrapper "basic"

$digit = 0-9

$alpha = [a-zA-Z]

tokens :-
    $white+	    ;
    let	       	    { \_ -> TokenLet }
    in	       	    { \_ -> TokenIn }
    \=	       	    { \_ -> TokenEq }

    \+		    { \_ -> TokenPlus }
    \-	       	    { \_ -> TokenMinus }
    \*	       	    { \_ -> TokenStar }

    \<		    { \_ -> TokenLt }

    \++	       	    { \_ -> TokenPlusPlus }

    \(		    { \_ -> TokenLP }
    \)	       	    { \_ -> TokenRP }

    \\		    { \_ -> TokenBackslash }
    \->		    { \_ -> TokenArr }

    match	    { \_ -> TokenMatch }
    \|		    { \_ -> TokenBar }
    end		    { \_ -> TokenEnd }

    $alpha+    	    { \s -> TokenId s }
    \" [^\"]* \"    { \s -> TokenStr (read s) }
    $digit+    	    { \s -> TokenInt (read s) }

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
  | TokenEnd

  | TokenId String
  | TokenStr String
  | TokenInt Int
  deriving (Eq, Show)

lex :: String -> [Token]
lex = alexScanTokens
}
