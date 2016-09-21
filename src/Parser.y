{
module Parser where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  '(' { TokenLParen $$ }
  ')' { TokenRParen $$ }
  '[' { TokenLBrack $$ }
  ']' { TokenRBrack $$ }
  '{' { TokenLBrace $$ }
  '}' { TokenRBrace $$ }
  ':=' { TokenAssign $$ }
  ':' { TokenColon $$ }
  ',' { TokenComma $$ }
  '#' { TokenHash $$ }
  '>=' { TokenGE $$ }
  '<=' { TokenLE $$ }
  '>' { TokenGT $$ }
  '<' { TokenLT $$ }
  '==' { TokenEQ $$ }
  '!=' { TokenNEQ $$ }
  '+' { TokenPlus $$ }
  '-' { TokenMinus $$ }
  '*' { TokenTimes $$ }
  TkBool { TokenBool $$ }
  TkInt { TokenInt $$ }
  TkVec { TokenVec $$ }
  TkTrue { TokenTrue $$ }
  TkFalse { TokenFalse $$ }
  TkNot { TokenNot $$ }
  TkAnd { TokenAnd $$ }
  TkOr { TokenOr $$ }
  TkFun { TokenFun $$ }
  TkIf { TokenIf $$ }
  TkElse { TokenElse $$ }
  TkWhile { TokenWhile $$ }
  TkReturn { TokenReturn $$ }
  TkId { TokenId $$ }
  TkNum { TokenNum $$ }

%%

Program : {- empty -}                         { EmptyProgram }
        | FunctionList                        { Program $1 }
FunctionList : Function                       { [$1] }
        | Function FunctionList               { $1 : $2 }

Function : TkFun Id Params ':' Type Block     { Function $2 $5 $3 $6 }
      | TkFun Id Params Block                 { Function $2 Unit $3 $4 }

Params : '(' ParamList ')'                    { $2 }
ParamList : Param ',' ParamList               { $1 : $3 }
      | Param                                 { [$1] }
      | {- empty -}                           { [] }
Param : Id ':' Type                           { Parameter $1 $3 }

Block : '{' InstructionList '}'               { Block $2 }

InstructionList : {- empty -}                 { [] }
      | Instruction InstructionList           { $1 : $2 }
Instruction : Id ':=' Exp                     { StmtAssign $1 $3 }
      | VectorAssign                          { $1 }
      | TkIf Exp Block TkElse Block           { StmtIfElse $2 $3 $5 }
      | TkIf Exp Block                        { StmtIf $2 $3 }
      | TkWhile Exp Block                     { StmtWhile $2 $3 }
      | TkReturn Exp                          { StmtReturn $2 }

      | ProcCall                              { $1 }
ExpList : Exp ',' ExpList                     { $1 : $3 }
      | Exp                                   { [$1] }
      | {- empty -}                           { [] }
Exp : ExpLogic                                { $1 }
ExpLogic : ExpLogic TkAnd ExpLogicAtom        { ExprAnd $1 $3 }
      | ExpLogic TkOr ExpLogicAtom            { ExprOr $1 $3 }
      | ExpLogicAtom                          { $1 }
ExpLogicAtom : TkNot ExpLogicAtom             { ExprNot $2 }
      | ExpRelational                         { $1 }
ExpRelational : ExpSum '>=' ExpSum            { ExprGe $1 $3 }
      | ExpSum '<=' ExpSum                    { ExprLe $1 $3 }
      | ExpSum '>' ExpSum                     { ExprGt $1 $3 }
      | ExpSum '<' ExpSum                     { ExprLt $1 $3 }
      | ExpSum '==' ExpSum                    { ExprEq $1 $3 }
      | ExpSum '!=' ExpSum                    { ExprNe $1 $3 }
      | ExpSum                                { $1 }
ExpSum : ExpSum '+' ExpMul                    { ExprAdd $1 $3 }
      | ExpSum '-' ExpMul                     { ExprSub $1 $3 }
      | ExpMul                                { $1 }
ExpMul : ExpMul '*' ExpAtom                   { ExprMul $1 $3 }
      | ExpAtom                               { $1 }
ExpAtom : Id                                  { ExprVar $1 }
      | Num                                   { $1 }
      | Bool                                  { $1 }
      | Vector                                { $1 }
      | VectorSize                            { $1 }
      | VectorElem                            { $1 }
      | FuncCall                              { $1 }
      | '(' Exp ')'                           { $2 }
Type : TkBool                                 { Bool }
      | TkInt                                 { Int }
      | TkVec                                 { Vec }
Num : TkNum                                   { ExprConstNum (snd $1) }
Bool : TkTrue                                 { ExprConstBool True }
  | TkFalse                                   { ExprConstBool False }
Vector      :   '[' ExpList ']'               { ExprVecMake $2 }
VectorSize  :   '#' Id                        { ExprVecLength $2 }
VectorElem  :   Id '[' Exp ']'                { ExprVecDeref $1 $3 }
VectorAssign :  Id '[' Exp ']' ':=' Exp       { StmtVecAssign $1 $3 $6 }
ProcCall    :   Id '(' ExpList ')'            { StmtCall $1 $3 }
FuncCall    :   Id '(' ExpList ')'            { ExprCall $1 $3 }
Id          :   TkId                          { snd $1 }

{

parseError :: [Token] -> a
parseError (token:tokens) = let (item, line, column) = tokenPosn token in
  error ("Error de parseo: símbolo no válido \"" ++ item ++ "\" en linea " ++ (show line) ++ " columna " ++ (show column))

parsed :: String -> ProgramT
-- Parse the given input after tokenizing it
parsed input = parse (tokenize input)

type Id = String

data Type = Int | Bool | Vec | Unit deriving (Enum, Bounded, Eq, Show)

data ProgramT = EmptyProgram | Program [FunctionT]

data FunctionT = Function Id Type [ParameterT] BlockT deriving Show

data ParameterT = Parameter Id Type deriving Show

data BlockT = Block [StmtT] deriving Show

data StmtT = StmtAssign Id ExprT
        | StmtVecAssign Id ExprT ExprT
        | StmtIf ExprT BlockT
        | StmtIfElse ExprT BlockT BlockT
        | StmtWhile ExprT BlockT
        | StmtReturn ExprT
        | StmtCall Id [ExprT]
        deriving Show

data ExprT = ExprVar Id
        | ExprConstNum Integer
        | ExprConstBool Bool
        | ExprVecMake [ExprT]
        | ExprVecLength Id
        | ExprVecDeref Id ExprT
        | ExprCall Id [ExprT]
        | ExprAnd ExprT ExprT
        | ExprOr ExprT ExprT
        | ExprNot ExprT
        | ExprLe ExprT ExprT
        | ExprGe ExprT ExprT
        | ExprLt ExprT ExprT
        | ExprGt ExprT ExprT
        | ExprEq ExprT ExprT
        | ExprNe ExprT ExprT
        | ExprAdd ExprT ExprT
        | ExprSub ExprT ExprT
        | ExprMul ExprT ExprT
        deriving Show
}
