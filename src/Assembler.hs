module Assembler(
  assemble,
  Bytecode(..),
  BytecodeFunc(..),
  BytecodeValue(..),
  BytecodePrimitive(..),
  BytecodeStmt(..)
) where
import AssemblerHelper
import Control.Monad.State.Lazy
import Data.List

import Parser

--------------------------------------------------------------------------------
--                              EXPORTED                                      --
--------------------------------------------------------------------------------

assemble :: ProgramT -> Bytecode
assemble = assembleProgram

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------
assembleProgram :: ProgramT -> Bytecode
assembleProgram prog
  | (prog == EmptyProgram) ||
    (prog == (Program [Function "main" Unit [] (Block [])])) = BytecodeEmptyProgram
assembleProgram (Program funcs) = BytecodeProgram (evalState (mapS assembleFunction funcs) initialRegisterStatus)


----------- Functions -----------
assembleFunction :: FunctionT -> State RegisterStatus BytecodeFunc
assembleFunction funct@(Function ident _ _ block) = do
  prologue funct
  blockCmds <-assembleBlock block
  varSize <- localVarsSize
  return (BFunc ("cuca_" ++ ident)
    ((functionInitialization varSize) ++ blockCmds ++ (functionDeinitialization) ))

----------- Block -----------
assembleBlock :: BlockT -> State RegisterStatus [BytecodeStmt]
assembleBlock (Block stmts) = do
  blockCmds <- concatS $ mapS assembleStmt stmts
  return (blockCmds)

----------- Params -----------
assembleParam :: BytecodeValue -> (Int, ExprT) -> State RegisterStatus [BytecodeStmt]
assembleParam reg (i, expr@(ExprVecMake _)) = do
  argAddress <- argAddr i
  exprCmds <- assembleExpr reg expr
  return (
    (Add Rsp $ Const 8) ++=
    exprCmds ++
    (Sub Rsp $ Const 8) +=
    Mov argAddress reg)
assembleParam reg (i, expr) = do
  argAddress <- argAddr i
  exprCmds <- assembleExpr reg expr
  return (
    exprCmds +==
    Mov argAddress reg)

----------- Statements -----------
assembleStmt :: StmtT -> State RegisterStatus [BytecodeStmt]
-- putChar
assembleStmt (StmtCall "putChar" [expr] ) = do
  exprCmds <- (assembleExpr Rdi expr)
  return (
    exprCmds +==
    CCall CCPutChar)
-- putNum
assembleStmt (StmtCall "putNum" [expr] ) = do
  exprCmds <- (assembleExpr Rsi expr)
  return (
    exprCmds ++
    Mov Rdi LLIString ++=
    (Mov Rax $ Const 0) +=
    CCall CCPrintf)
-- parameterless call
assembleStmt (StmtCall name []) = do
  return ([Call $ "cuca_" ++ name])
-- call with parameters
assembleStmt (StmtCall name exprs) = do
  paramsCmds <- assembleParams Rdi exprs
  return (
    (Sub Rsp $ Const (word $ length exprs)) ++=
    paramsCmds ++
    (Call $ "cuca_" ++ name) +=
    (Add Rsp $ Const (word $ length exprs)) )
-- Asignments
assembleStmt (StmtAssign name expr) = do
  address <- addr name
  exprCmds <- assembleExpr Rdi expr
  return (
    exprCmds +==
    Mov address Rdi)
-- Returns
assembleStmt (StmtReturn expr) = do
  address <- returnAddr
  exprCmds <- assembleExpr Rdi expr
  return (
    exprCmds +==
    Mov address Rdi)
-- If without else
assembleStmt (StmtIf expr block) = do
  exprCmds <- assembleExpr Rsi expr
  blockCmds <- assembleBlock block
  i <- nextLabelIndex
  return (
    exprCmds ++
    (Cmp Rsi $ Const 0) ++=
    (Je $ label i "if_end") ++=
    blockCmds +==
    (Label $ label i "if_end"))
-- If with Else
assembleStmt (StmtIfElse expr block1 block2) = do
  exprCmds <- assembleExpr Rsi expr
  blockCmds1 <- assembleBlock block1
  blockCmds2 <- assembleBlock block2
  i <- nextLabelIndex
  return (
    exprCmds ++
    (Cmp Rsi $ Const 0) ++=
    (Je $ label i "if_else") ++=
    blockCmds1 ++
    (Jmp $ label i "if_end") ++=
    (Label $ label i "if_else") ++=
    blockCmds2 +==
    (Label $ label i "if_end"))
-- While
assembleStmt (StmtWhile expr block) = do
  exprCmds <- assembleExpr Rsi expr
  blockCmds <- assembleBlock block
  i <- nextLabelIndex
  return (
    (Label $ label i "while_start") ++=
    exprCmds ++
    (Cmp Rsi $ Const 0) ++=
    (Je $ label i "while_end") ++=
    blockCmds ++
    (Jmp $ label i "while_start") +=
    (Label $ label i "while_end"))
-- Vector Assignment
assembleStmt (StmtVecAssign name exprIdx expr) = do
  exprCmds <- assembleExpr Rdi expr
  exprIdxCmds <- assembleExpr Rsi exprIdx
  address <- addr name
  return (
    exprCmds ++
    exprIdxCmds ++
    Mov Rax Rsi ++=
    Inc Rax ++=
    (Sal Rax $ Const 3) ++=
    Add Rax address +=
    Mov (Mem Rax "+" 0) Rdi)

----------- Expressions -----------
-- Numeric Constants
assembleExpr reg (ExprConstNum n) = do
  return [Mov reg $ Const n]
-- Boolean Constants
assembleExpr reg (ExprConstBool False) = do
  return [Mov reg $ Const 0]
assembleExpr reg (ExprConstBool True) = do
  return [Mov reg $ Const (-1)]
-- Variables or params
assembleExpr reg (ExprVar name) = do
  address <- addr name
  return (
    (usingTemporary $ Mov reg address))
-- Addition
assembleExpr reg (ExprAdd exp1 exp2) = do
  (exprs, next) <- assembleBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Add reg next))
-- Subtraction
assembleExpr reg (ExprSub exp1 exp2) = do
  (exprs, next) <- assembleBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Sub reg next))
-- Multiplication
assembleExpr reg (ExprMul exp1 exp2) = do
  (exprs, next) <- assembleBinaryExpr reg exp1 exp2
  return (
    exprs ++
    Mov Rax reg ++=
    Mul next ++=
    (usingTemporary $ Mov reg Rax))
-- Boolean AND
assembleExpr reg (ExprAnd exp1 exp2) = do
  (exprs, next) <- assembleBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ And reg next))
-- Boolean OR
assembleExpr reg (ExprOr exp1 exp2) = do
  (exprs, next) <- assembleBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Or reg next))
-- Boolean NOT
assembleExpr reg (ExprNot expr) = do
  exprs <- assembleExpr reg expr
  return (
    exprs ++
    (usingTemporary $ Not reg))
-- Comparisons
assembleExpr reg (ExprEq expr1 expr2) = assembleComparison reg expr1 expr2 Je
assembleExpr reg (ExprNe expr1 expr2) = assembleComparison reg expr1 expr2 Jne
assembleExpr reg (ExprGt expr1 expr2) = assembleComparison reg expr1 expr2 Jg
assembleExpr reg (ExprLt expr1 expr2) = assembleComparison reg expr1 expr2 Jl
assembleExpr reg (ExprGe expr1 expr2) = assembleComparison reg expr1 expr2 Jge
assembleExpr reg (ExprLe expr1 expr2) = assembleComparison reg expr1 expr2 Jle
--Function Calls
assembleExpr reg (ExprCall name exprs) = do
  paramsCmds <- assembleParams Rdi exprs
  address <- returnAddr
  usedPush <- usedRegsPush
  usedPop <- usedRegsPop
  paramsCmds <- assembleParams Rdi exprs
  return (
    usedPush ++
    (Sub Rsp $ Const (word $ length exprs)) ++=
    paramsCmds ++
    (Call $ "cuca_" ++ name) ++=
    (Add Rsp $ Const (word $ length exprs)) ++=
    usedPop +==
    Mov reg Rax)
-- Vector Creation
assembleExpr reg (ExprVecMake exprs) = do
  vectorItems <- assembleVectorExprs Rdi Rsi exprs
  sizeAddr <- vecAddr Rdi 0
  return (
    Sub Rsp (Const $ word $ length exprs + 1) ++=
    Mov Rdi Rsp ++=
    (MovQ sizeAddr (Const $ length exprs)) ++=
    vectorItems)
-- Vector Length
assembleExpr reg (ExprVecLength name) = do
  address <- addr name
  return (
    Mov Rax address +=
    Mov reg (Mem Rax "+" 0))
-- Vector Deref
assembleExpr reg (ExprVecDeref name expr) = do
  exprCmds <- assembleExpr reg expr
  address <- addr name
  return (
    exprCmds ++
    Mov Rax reg ++=
    Inc Rax ++=
    (Sal Rax $ Const 3) ++=
    Add Rax address +=
    Mov reg (Mem Rax "+" 0))


--------------------------------------------------------------------------------
--                              INIT & DEINIT                                 --
--------------------------------------------------------------------------------
-- When a function starts
functionInitialization :: Int -> [BytecodeStmt]
functionInitialization varSize =
    Push Rbp ++=
    Mov Rbp Rsp ++=
    (Sub Rsp $ Const varSize) +=
    Comment "End initialization"

-- When a function finishes
functionDeinitialization :: [BytecodeStmt]
functionDeinitialization =
    Comment "Start deinitialization" ++=
    Mov Rsp Rbp ++=
    Pop Rbp +=
    Ret

--------------------------------------------------------------------------------
--                HELPER FOR ACTIONS THAT NEED TEMPORARY REG                  --
--------------------------------------------------------------------------------
-- Avoids Memory to Memory operations or in Mmemory operations that cannot be
-- performed directly by using a temporary register, if needed
usingTemporary :: BytecodeStmt -> [BytecodeStmt]
usingTemporary bop =
  let tr = Rdx
  in case bop of
    (Mov reg1@(Mem _ _ _) reg2@(Mem _ _ _)) -> [Mov tr reg2, Mov reg1 tr]
    (Add reg1@(Mem _ _ _) reg2@(Mem _ _ _)) -> [Mov tr reg1, Add tr reg2, Mov reg1 tr]
    (Sub reg1@(Mem _ _ _) reg2@(Mem _ _ _)) -> [Mov tr reg1, Sub tr reg2, Mov reg1 tr]
    (And reg1@(Mem _ _ _) reg2@(Mem _ _ _)) -> [Mov tr reg1, And tr reg2, Mov reg1 tr]
    (Or reg1@(Mem _ _ _) reg2@(Mem _ _ _))  -> [Mov tr reg1, Or tr reg2, Mov reg1 tr]
    (Not reg1@(Mem _ _ _))                  -> [Mov tr reg1, Not tr, Mov reg1 tr]
    otherwise -> [bop]

--------------------------------------------------------------------------------
--                              ADRESSES                                      --
--------------------------------------------------------------------------------
-- Return address
returnAddr :: State RegisterStatus BytecodeValue
returnAddr = do
  return Rax

-- Address to set the i-th argument before calling a function
argAddr :: Int -> State RegisterStatus BytecodeValue
argAddr i = do
  return (Mem Rsp "+" $ word i)

-- Vector element setting address
vecAddr :: BytecodeValue -> Int -> State RegisterStatus BytecodeValue
vecAddr reg i = do
  return (Mem reg "+" $ word i)

-- Address to fetch a parameter value in a function
paramAddr :: String -> State RegisterStatus BytecodeValue
paramAddr name = do
  idx <- paramIndex name
  return (Mem Rbp "+" idx)

-- Address to fetch/set a local variable
varAddr :: String -> State RegisterStatus BytecodeValue
varAddr name = do
  idx <- varIndex name
  return (Mem Rbp "-" idx)

-- Generic not caring if it's param or local variable
addr :: String -> State RegisterStatus BytecodeValue
addr name = do
  param <- isParam name
  if param then paramAddr name else varAddr name

--------------------------------------------------------------------------------
--                          GENERAL LIST HELPERS                              --
--------------------------------------------------------------------------------
-- a list of parameters passed to a function
assembleParams :: BytecodeValue -> [ExprT] -> State RegisterStatus [BytecodeStmt]
assembleParams reg exprs = concatS $ mapS (assembleParam reg) (withIndices exprs)

-- a list of expressions in a vector construction
assembleVectorExprs :: BytecodeValue -> BytecodeValue -> [ExprT] -> State RegisterStatus [BytecodeStmt]
assembleVectorExprs regV reg exprs = concatS $ mapS (assembleVectorExpr regV reg) (withIndices exprs)

--------------------------------------------------------------------------------
--                             GENERAL HELPERS                                --
--------------------------------------------------------------------------------
-- Vector
assembleVectorExpr regV reg (i, expr) = do
  argAddress <- vecAddr regV (i+1)
  exprCmds <- assembleExpr reg expr
  return (
    exprCmds +==
    MovQ argAddress reg)

usedRegsDo f registers = map (\r -> f r) registers
usedRegsPush = do
  regs <- getUsedRegisters
  return (usedRegsDo Push $ regs)
usedRegsPop = do
  regs <- getUsedRegisters
  return (usedRegsDo Pop $ reverse regs)

assembleBinaryExpr reg exp1 exp2 = do
  expr1cmds <- assembleExpr reg exp1
  next <- nextRegister
  expr2cmds <- assembleExpr next exp2
  freeRegister next
  return (expr1cmds ++ expr2cmds, next)

assembleComparison reg expr1 expr2 jump = do
  exprsCmds1 <- assembleExpr Rsi expr1
  exprsCmds2 <- assembleExpr Rdi expr2
  i <- nextLabelIndex
  return (
    exprsCmds1 ++
    exprsCmds2 ++
    (Mov Rdx $ Const (-1)) ++=
    (Cmp Rsi Rdi) ++=
    (jump $ label i "cmp_end") ++=
    (Mov Rdx $ Const 0) ++=
    (Label $ label i "cmp_end") +=
    (Mov reg Rdx))
