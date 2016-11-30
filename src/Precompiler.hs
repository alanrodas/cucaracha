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

data BytecodeStmt = BPutChar Int | BPutNum Int | None

precompile EmptyProgram = BytecodeEmptyProgram
precompile (Program [Function "main" Unit [] (Block [])]) = BytecodeEmptyProgram
precompile (Program funcs) = BytecodeProgram (map precompileFunction funcs)

precompileFunction (Function ident Unit [] (Block stmts)) = BFunc ("cuca_" ++ ident) (map precompileStmt stmts)

precompileStmt (StmtCall "putChar" exprs) = BPutChar 2
precompileStmt (StmtCall "putNum" exprs) = BPutNum 1
precompileStmt (StmtCall other exprs) = None
