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
assemble = precompileProgram

--------------------------------------------------------------------------------
--                              INTERNAL                                      --
--------------------------------------------------------------------------------

---------- Initialization
-------------------------


---------- Program
------------------
precompileProgram prog
  | (prog == EmptyProgram) ||
    (prog == (Program [Function "main" Unit [] (Block [])])) = BytecodeEmptyProgram
precompileProgram (Program funcs) = BytecodeProgram (evalState (mapS precompileFunction funcs) initialRegisterStatus)


---------- Functions
--------------------

precompileFunction funct@(Function ident _ _ block) = do
  prologue funct
  blockCmds <-precompileBlock block
  varSize <- localVarsSize
  return (BFunc ("cuca_" ++ ident)
    ((functionInitialization varSize) ++ blockCmds ++ (functionDeinitialization) ))

---------- Block
----------------
precompileBlock (Block stmts) = do
  blockCmds <- concatS $ mapS precompileStmt stmts
  return (blockCmds)


---------- Parameters
----------------------
-- a list of parameters
precompileParams reg exprs = concatS $ mapS (precompileParam reg) (withIndices exprs)

-- Constants
precompileParam reg (i, expr) = do
  argAddress <- argAddr i
  exprCmds <- precompileExpr reg expr
  return (exprCmds ++ [Mov argAddress reg])

---------- Statements
---------------------
-- Calls
    -- putChar
precompileStmt (StmtCall "putChar" [expr] ) = do
  exprCmds <- (precompileExpr Rdi expr)
  return (
    exprCmds +==
    CCall CCPutChar)
    -- putNum
precompileStmt (StmtCall "putNum" [expr] ) = do
  exprCmds <- (precompileExpr Rsi expr)
  return (
    exprCmds ++
    Mov Rdi LLIString ++=
    (Mov Rax $ Const 0) +=
    CCall CCPrintf)
      -- parameterless call
precompileStmt (StmtCall name []) = do
  return ([Call $ "cuca_" ++ name])
      -- call with N parameters
precompileStmt (StmtCall name exprs) = do
  paramsCmds <- precompileParams Rdi exprs
  return (
    (Sub Rsp $ Const (word $ length exprs)) ++=
    paramsCmds ++
    (Call $ "cuca_" ++ name) +=
    (Add Rsp $ Const (word $ length exprs)) )
-- Asignments
precompileStmt (StmtAssign name expr) = do
  address <- addr name
  exprCmds <- precompileExpr Rdi expr
  return (
    exprCmds +==
    (Commented (name ++ " assignment") $ Mov address Rdi))
-- Returns
precompileStmt (StmtReturn expr) = do
  address <- returnAddr
  exprCmds <- precompileExpr Rdi expr
  return (
    exprCmds +==
    (Commented "return" $ Mov address Rdi))
-- If no else
precompileStmt (StmtIf expr block) = do
  exprCmds <- precompileExpr Rsi expr
  blockCmds <- precompileBlock block
  i <- nextLabelIndex
  return (
    Comment "if start" ++=
    exprCmds ++
    (Commented "if comparison" $ Cmp Rsi $ Const 0) ++=
    (Je $ label i "if_end") ++=
    Comment "if_then" ++=
    blockCmds +==
    (Label $ label i "if_end"))
precompileStmt (StmtIfElse expr block1 block2) = do
  exprCmds <- precompileExpr Rsi expr
  blockCmds1 <- precompileBlock block1
  blockCmds2 <- precompileBlock block2
  i <- nextLabelIndex
  return (
    Comment "if start" ++=
    exprCmds ++
    (Commented "if comparison" $ Cmp Rsi $ Const 0) ++=
    (Je $ label i "if_else") ++=
    Comment "if_then" ++=
    blockCmds1 ++
    (Jmp $ label i "if_end") ++=
    (Label $ label i "if_else") ++=
    blockCmds2 +==
    (Label $ label i "if_end"))
precompileStmt (StmtWhile expr block) = do
  exprCmds <- precompileExpr Rsi expr
  blockCmds <- precompileBlock block
  i <- nextLabelIndex
  return (
    (Label $ label i "while_start") ++=
    exprCmds ++
    (Commented "while comparison" $ Cmp Rsi $ Const 0) ++=
    (Je $ label i "while_end") ++=
    blockCmds ++
    (Jmp $ label i "while_start") +=
    (Label $ label i "while_end"))







---------- Expressions
----------------------
-- Constants
precompileExpr reg (ExprConstNum n) = do
  return [Commented (show n) $ Mov reg $ Const n]
precompileExpr reg (ExprConstBool False) = do
  return [Commented "false" $ Mov reg $ Const 0]
precompileExpr reg (ExprConstBool True) = do
  return [Commented "true" $ Mov reg $ Const (-1)]
-- Variables or params
precompileExpr reg (ExprVar name) = do
  address <- addr name
  return (
    Comment("fetch " ++ name) ++=
    (usingTemporary $ Mov reg address))
-- Addition
precompileExpr reg (ExprAdd exp1 exp2) = do
  (exprs, next) <- precompileBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Add reg next))
-- Subtraction
precompileExpr reg (ExprSub exp1 exp2) = do
  (exprs, next) <- precompileBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Sub reg next))
-- Multiplication
precompileExpr reg (ExprMul exp1 exp2) = do
  (exprs, next) <- precompileBinaryExpr reg exp1 exp2
  return (
    exprs ++
    Mov Rax reg ++=
    Mul next ++=
    (usingTemporary $ Mov reg Rax))
-- And
precompileExpr reg (ExprAnd exp1 exp2) = do
  (exprs, next) <- precompileBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ And reg next))
-- Or
precompileExpr reg (ExprOr exp1 exp2) = do
  (exprs, next) <- precompileBinaryExpr reg exp1 exp2
  return (
    exprs ++
    (usingTemporary $ Or reg next))
-- Not
precompileExpr reg (ExprNot expr) = do
  exprs <- precompileExpr reg expr
  return (
    exprs ++
    (usingTemporary $ Not reg))
-- Comparisons
precompileExpr reg (ExprEq expr1 expr2) = precompileComparison reg expr1 expr2 Je
precompileExpr reg (ExprNe expr1 expr2) = precompileComparison reg expr1 expr2 Jne
precompileExpr reg (ExprGt expr1 expr2) = precompileComparison reg expr1 expr2 Jg
precompileExpr reg (ExprLt expr1 expr2) = precompileComparison reg expr1 expr2 Jl
precompileExpr reg (ExprGe expr1 expr2) = precompileComparison reg expr1 expr2 Jge
precompileExpr reg (ExprLe expr1 expr2) = precompileComparison reg expr1 expr2 Jle
--Function Calls
precompileExpr reg (ExprCall name exprs) = do
  paramsCmds <- precompileParams Rdi exprs
  address <- returnAddr
  usedPush <- usedRegsPush
  usedPop <- usedRegsPop
  paramsCmds <- precompileParams Rdi exprs
  return (
    Comment "Function call pre-saving registers" ++=
    usedPush ++
    (Sub Rsp $ Const (word $ length exprs)) ++=
    paramsCmds ++
    (Call $ "cuca_" ++ name) ++=
    (Add Rsp $ Const (word $ length exprs)) ++=
    usedPop ++
    Mov reg Rax +=
    Comment "Ending function call")
-- Vectors
precompileExpr reg (ExprVecMake exprs) = do
  vectorItems <- precompileVectorExprs Rsi exprs
  sizeAddr <- vecAddr Rsi 0
  return (
    Sub Rsp (Const $ word $ length exprs + 1) ++=
    Mov Rdi Rsp ++=
    (MovQ sizeAddr (Const $ length exprs)) ++=
    vectorItems)

-- a vector construction
precompileVectorExprs reg exprs = concatS $ mapS (precompileVectorExpr reg) (withIndices exprs)

-- Vector
precompileVectorExpr reg (i, expr) = do
  argAddress <- vecAddr Rsi (i+1)
  exprCmds <- precompileExpr reg expr
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

precompileBinaryExpr reg exp1 exp2 = do
  expr1cmds <- precompileExpr reg exp1
  next <- nextRegister
  expr2cmds <- precompileExpr next exp2
  freeRegister next
  return (expr1cmds ++ expr2cmds, next)

precompileComparison reg expr1 expr2 jump = do
  exprsCmds1 <- precompileExpr Rsi expr1
  exprsCmds2 <- precompileExpr Rdi expr2
  i <- nextLabelIndex
  return (
    exprsCmds1 ++
    exprsCmds2 ++
    Comment "Start a numeric comparisson" ++=
    (Mov Rdx $ Const (-1)) ++=
    (Commented "Comparing" $ Cmp Rsi Rdi) ++=
    (jump $ label i "cmp_end") ++=
    (Mov Rdx $ Const 0) ++=
    (Label $ label i "cmp_end") +=
    (Commented "Compared in reg" $ Mov reg Rdx))

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
--                             GENERAL HELPERS                                --
--------------------------------------------------------------------------------
