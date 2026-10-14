{
module Ark.Surface.Lexer ( Token(..), lexTokens ) where
}

%wrapper "basic"

$digit = 0-9

tokens :-
    $white+    ;
    \+	       { \_ -> TokenPlus }
    \-	       { \_ -> TokenMinus }
    \(	       { \_ -> TokenLP }
    \)	       { \_ -> TokenRP }
    $digit+    { \s -> TokenInt (read s) }

{
data Token
  = TokenPlus
  | TokenMinus
  | TokenLP
  | TokenRP
  | TokenInt Int
  deriving (Eq, Show)

lexTokens = alexScanTokens
}
