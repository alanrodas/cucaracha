{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]
$alphanumeric = [$alpha $digit]

tokens :-
  $white+                      ;
  \/\/.*                       ;
  \(                           { \p s -> TokenLParen p }
  \)                           { \p s -> TokenRParen p }
  \[                           { \p s -> TokenLBrack p }
  \]                           { \p s -> TokenRBrack p }
  \{                           { \p s -> TokenLBrace p }
  \}                           { \p s -> TokenRBrace p }
  \:\=                         { \p s -> TokenAssign p }
  \:                           { \p s -> TokenColon p }
  \,                           { \p s -> TokenComma p }
  \#                           { \p s -> TokenHash p }
  \>\=                         { \p s -> TokenGE p }
  \<\=                         { \p s -> TokenLE p }
  \>                           { \p s -> TokenGT p }
  \<                           { \p s -> TokenLT p }
  \=\=                         { \p s -> TokenEQ p }
  \!\=                         { \p s -> TokenNEQ p }
  \+                           { \p s -> TokenPlus p }
  \-                           { \p s -> TokenMinus p }
  \*                           { \p s -> TokenTimes p }
  Bool                         { \p s -> TokenBool p }
  Int                          { \p s -> TokenInt p }
  Vec                          { \p s -> TokenVec p }
  True                         { \p s -> TokenTrue p }
  False                        { \p s -> TokenFalse p }
  not                          { \p s -> TokenNot p }
  and                          { \p s -> TokenAnd p }
  or                           { \p s -> TokenOr p }
  fun                          { \p s -> TokenFun p }
  if                           { \p s -> TokenIf p }
  else                         { \p s -> TokenElse p }
  while                        { \p s -> TokenWhile p }
  return                       { \p s -> TokenReturn p }
  $alpha [$alphanumeric \_]*   { \p s -> TokenId (p, s) }
  $digit+                      { \p s -> TokenNum (p, (read s)) }

{

-- The token type:
data Token =
             TokenLParen AlexPosn
           | TokenRParen AlexPosn
           | TokenLBrack AlexPosn
           | TokenRBrack AlexPosn
           | TokenLBrace AlexPosn
           | TokenRBrace AlexPosn
           | TokenAssign AlexPosn
           | TokenColon AlexPosn
           | TokenComma AlexPosn
           | TokenHash AlexPosn
           | TokenGE AlexPosn
           | TokenLE AlexPosn
           | TokenGT AlexPosn
           | TokenLT AlexPosn
           | TokenEQ AlexPosn
           | TokenNEQ AlexPosn
           | TokenPlus AlexPosn
           | TokenMinus AlexPosn
           | TokenTimes AlexPosn
           | TokenBool AlexPosn
           | TokenInt AlexPosn
           | TokenVec AlexPosn
           | TokenTrue AlexPosn
           | TokenFalse AlexPosn
           | TokenNot AlexPosn
           | TokenAnd AlexPosn
           | TokenOr AlexPosn
           | TokenFun AlexPosn
           | TokenIf AlexPosn
           | TokenElse AlexPosn
           | TokenWhile AlexPosn
           | TokenReturn AlexPosn
           | TokenId (AlexPosn, String)
           | TokenNum (AlexPosn, Integer)
           deriving (Eq,Show)

tokenize = alexScanTokens
}
