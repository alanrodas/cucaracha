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

tokenPosn :: Token -> (String, Int, Int)
-- Retrieve the token string representation and its current position (line, column)
tokenPosn (TokenLParen (AlexPn offset line column)) = ("(", line, column)
tokenPosn (TokenRParen (AlexPn offset line column)) = (")", line, column)
tokenPosn (TokenLBrack (AlexPn offset line column)) = ("[", line, column)
tokenPosn (TokenRBrack (AlexPn offset line column)) = ("]", line, column)
tokenPosn (TokenLBrace (AlexPn offset line column)) = ("{", line, column)
tokenPosn (TokenRBrace (AlexPn offset line column)) = ("}", line, column)
tokenPosn (TokenAssign (AlexPn offset line column)) = (":=", line, column)
tokenPosn (TokenColon (AlexPn offset line column)) = (":", line, column)
tokenPosn (TokenComma (AlexPn offset line column)) = (",", line, column)
tokenPosn (TokenHash (AlexPn offset line column)) = ("#", line, column)
tokenPosn (TokenGE (AlexPn offset line column)) = (">=", line, column)
tokenPosn (TokenLE (AlexPn offset line column)) = ("<=", line, column)
tokenPosn (TokenGT (AlexPn offset line column)) = (">", line, column)
tokenPosn (TokenLT (AlexPn offset line column)) = ("<", line, column)
tokenPosn (TokenEQ (AlexPn offset line column)) = ("==", line, column)
tokenPosn (TokenNEQ (AlexPn offset line column)) = ("!=", line, column)
tokenPosn (TokenPlus (AlexPn offset line column)) = ("+", line, column)
tokenPosn (TokenMinus (AlexPn offset line column)) = ("-", line, column)
tokenPosn (TokenTimes (AlexPn offset line column)) = ("*", line, column)
tokenPosn (TokenBool (AlexPn offset line column)) = ("Bool", line, column)
tokenPosn (TokenInt (AlexPn offset line column)) = ("Int", line, column)
tokenPosn (TokenVec (AlexPn offset line column)) = ("Vec", line, column)
tokenPosn (TokenTrue (AlexPn offset line column)) = ("True", line, column)
tokenPosn (TokenFalse (AlexPn offset line column)) = ("False", line, column)
tokenPosn (TokenNot (AlexPn offset line column)) = ("not", line, column)
tokenPosn (TokenAnd (AlexPn offset line column)) = ("and", line, column)
tokenPosn (TokenOr (AlexPn offset line column)) = ("or", line, column)
tokenPosn (TokenFun (AlexPn offset line column)) = ("fun", line, column)
tokenPosn (TokenIf (AlexPn offset line column)) = ("if", line, column)
tokenPosn (TokenElse (AlexPn offset line column)) = ("else", line, column)
tokenPosn (TokenWhile (AlexPn offset line column)) = ("while", line, column)
tokenPosn (TokenReturn (AlexPn offset line column)) = ("return", line, column)
tokenPosn (TokenId (AlexPn offset line column, item)) = (item, line, column)
tokenPosn (TokenNum (AlexPn offset line column, item)) = (show item, line, column)

}
