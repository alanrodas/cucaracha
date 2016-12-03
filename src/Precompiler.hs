module Precompiler where
import Parser

{-
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

-}
data Bytecode = BytecodeEmptyProgram | BytecodeProgram [BytecodeFunc]

data BytecodeFunc = BFunc String [BytecodeStmt]

data Register = LLIString | Rdi | Rsi | Rax

data CCallPrimitive = CCPutChar | CCPrintf | CCExit

data BytecodeStmt = Mov Register Integer |
                    Movr Register Register |
                    Call String |
                    CCall CCallPrimitive |
                    Ret |
                    None

precompile EmptyProgram = BytecodeEmptyProgram
precompile (Program [Function "main" Unit [] (Block [])]) = BytecodeEmptyProgram
precompile (Program funcs) = BytecodeProgram (map precompileFunction funcs)

precompileFunction (Function ident Unit [] (Block stmts)) =
  BFunc ("cuca_" ++ ident) (precompileStmts stmts ++ [Ret])

precompileStmts stmts = concat (map precompileStmt stmts)

precompileStmt (StmtCall "putChar" [(ExprConstNum n)] ) = [Mov Rdi n, CCall CCPutChar]
precompileStmt (StmtCall "putNum" [(ExprConstNum n)] ) = [Mov Rsi n, Movr Rdi LLIString, Mov Rax 0,CCall CCPrintf]
precompileStmt (StmtCall name exprs) = [Call name]

precompileExprs exprs = concat (map precompileExpr exprs)

precompileExpr (ExprConstNum n) = [Mov Rdi n]
