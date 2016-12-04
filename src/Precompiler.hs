module Precompiler(
  precompile,
  Bytecode(..),
  BytecodeFunc(..),
  Register(..),
  CCallPrimitive(..),
  Memory(..),
  BytecodeStmt(..)
) where
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as Map

import Parser
--------------------------------------------------------------------------------
--                              EXPORTED                                      --
--------------------------------------------------------------------------------

-- Precompile a program
precompile EmptyProgram = BytecodeEmptyProgram
-- An empty main is considered an empty program just to save some bytes
precompile (Program [Function "main" Unit [] (Block [])]) = BytecodeEmptyProgram
-- Compile the program
precompile (Program funcs) = BytecodeProgram (map (precompileFunction Nothing) funcs)

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
-- Declare output assembly AST types
data Bytecode = BytecodeEmptyProgram | BytecodeProgram [BytecodeFunc]

data BytecodeFunc = BFunc String [BytecodeStmt]

data Register = LLIString -- Used for primitive call to printf
              | Rdi | Rsi | Rax -- Used for primitives PutChar and PutNum, do not use for anything else
              | Rbx | Rcx | Rdx  -- General purpose
              | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -- General purpose
              | Rbp | Rsp -- Reserved for stack management

-- Primitive function calls
data CCallPrimitive = CCPutChar | CCPrintf | CCExit

data Memory = Mem Register String Int | Memr Register

data BytecodeStmt = Mov Register Int |
                    Movr Register Register |
                    Movtm Memory Register |
                    Movfm Register Memory |
                    Call String |
                    CCall CCallPrimitive |
                    Ret |
                    Push Int |
                    Pushr Register |
                    Popr Register |
                    Sub Register Int |
                    Add Register Int |
                    None

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------

-- The function prologue to hold useful values of the function
data FunctionPrologue = FPrologue {
      name :: String,
      localVariables :: [String],
      parameters :: [String],
      maxExpressionNesting :: Int
}

-- Calculare a function's prologue
prologue (Function name _ parameters block) =
  FPrologue {
    name = name,
    parameters = map (\(Parameter name _) -> name) parameters,
    localVariables = variablesInBlock block,
    maxExpressionNesting = 0
  }

variablesInBlock (Block statements) = cmap variablesInStatement statements
variablesInStatement (StmtAssign name _) = [name]
variablesInStatement (StmtIf _ block) = variablesInBlock block
variablesInStatement (StmtIfElse _ block1 block2) =
  nub (variablesInBlock block1 ++ variablesInBlock block2)
variablesInStatement (StmtWhile _ block) = variablesInBlock block
variablesInStatement _ = []

isParam pf name = elemIndex name (parameters pf) /= Nothing
paramIndex pf name = let Just x = elemIndex name (parameters pf) in x

isVariable pf name = elemIndex name (localVariables pf) /= Nothing
varIndex pf name = let Just x = elemIndex name (localVariables pf) in x

--------------------------------------------------------------------------------
{-
data RegisterStatus = RegStatus {
  availableRegisters :: [Register],
  mappedRegisters :: Map.Map String Register
}

initialRegisterStatus = RegStatus {
  availableRegisters = [Rbx, Rcx, R9, R10, R11, R12, R13, R14, R15],
  mappedRegisters = Map.empty
}

mapRegister name = do
  currState <- get
  let x:xs = availableRegisters currState
  let mapped = mappedRegisters currState
  return RegStatus {
    availableRegisters = xs,
    mappedRegisters = Map.insert name x mapped
  }

registerFor name = do
  currState <- get
  let mapped = mappedRegisters currState
  return mapped Map.! name

isMapped name = do
  currState <- get
  let mapped = mappedRegisters currState
  return Map.member name mapped
-}
--------------------------------------------------------------------------------


-- The word size
word n = 8 * n

-- Helpers
withIndices ls = zip ([0..(length ls)]) ls
cmap f ls = concat (map f ls)


-- The main function does not require push and pop to the stack
precompileFunction s funct@(Function "main" _ _ (Block stmts)) = let pf = (prologue funct)
  in BFunc ("cuca_main") (precompileStmts s pf stmts ++ [Ret])
precompileFunction s funct@(Function ident _ _ (Block stmts)) = let pf = (prologue funct)
  in BFunc ("cuca_" ++ ident) (functionInitialization pf ++
      precompileStmts s pf stmts ++ functionDeinitialization pf )

functionInitialization pf = [Pushr Rbp, Movr Rbp Rsp, Sub Rsp (word (length (localVariables pf)))]
functionDeinitialization pf = [Movr Rbp Rsp, Popr Rbp, Ret]

precompileStmts s pf stmts = cmap (precompileStmt s pf) stmts

precompileStmt s pf (StmtCall "putChar" exprs ) =
  (precompileForPrimitiveExprs pf Rdi exprs) ++ [CCall CCPutChar]
precompileStmt s pf (StmtCall "putNum" exprs ) =
  (precompileForPrimitiveExprs pf Rsi exprs) ++ [Movr Rdi LLIString, Mov Rax 0,CCall CCPrintf]
precompileStmt s pf (StmtCall name []) =
  [Call name]
precompileStmt s pf (StmtCall name exprs) =
  [Sub Rsp (word (length exprs))] ++
  (precompileParams s pf Rdi exprs) ++
  [Call name] ++
  [Add Rsp (word (length exprs))]


precompileParams s pf reg exprs = cmap (precompileParam s pf reg) (withIndices exprs)
precompileParam s pf reg (i, (ExprConstNum n)) = [Mov reg n, Movtm (Mem Rsp "+" (word i)) reg]
precompileParam s pf reg (i, (ExprConstBool False)) = [Mov reg 0, Movtm (Mem Rsp "+" (word i)) reg]
precompileParam s pf reg (i, (ExprConstBool True)) = [Mov reg (-1), Movtm (Mem Rsp "+" (word i)) reg]
precompileParam s pf reg (i, (ExprVar name)) = do
  if isParam pf name then
    [Movfm reg (Mem Rbp "+" ((word (paramIndex pf name) + 16))),
    Movtm (Mem Rsp "+" (word i)) reg]
  else
    []
-- precompileParam pf reg (i, (ExprVar name)) = (elemIndex name (parameters pf)) * 8

--precompileForPrimitiveExprs pf reg exprs = concat (map (precompileExpr pf reg) exprs)

precompileForPrimitiveExprs pf reg [(ExprConstNum n)] = [Mov reg n]
precompileForPrimitiveExprs pf reg [(ExprConstBool False)] = [Mov reg 0]
precompileForPrimitiveExprs pf reg [(ExprConstBool True)] = [Mov reg (-1)]
precompileForPrimitiveExprs pf reg [(ExprVar name)] =
  if isParam pf name then
    [Movfm reg (Mem Rbp "+" ((word (paramIndex pf name) + 16)))]
  else
    [Add R9 3]
