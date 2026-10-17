{
module Ark.Lexer ( Token(..), lexTokens ) where
}

%wrapper "basic"

$digit = 0-9

$alpha = [a-zA-Z]

tokens :-
    $white+    ;
    let	       { \_ -> TokenLet }
    in	       { \_ -> TokenIn }
    \=	       { \_ -> TokenEq }
    \+	       { \_ -> TokenPlus }
    \-	       { \_ -> TokenMinus }
    \(	       { \_ -> TokenLP }
    \)	       { \_ -> TokenRP }
    $alpha+    { \s -> TokenId s }
    $digit+    { \s -> TokenInt (read s) }

{
data Token
  = TokenLet
  | TokenIn
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenLP
  | TokenRP
  | TokenId String
  | TokenInt Int
  deriving (Eq, Show)

lexTokens = alexScanTokens
}
