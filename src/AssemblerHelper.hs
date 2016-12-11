module AssemblerHelper where

import Control.Monad.State.Lazy
import Data.List
import Parser

--------------------------------------------------------------------------------
--                          ASSEMBLY AST TYPES                                --
--------------------------------------------------------------------------------
data Bytecode = BytecodeEmptyProgram | BytecodeProgram [BytecodeFunc]

data BytecodeFunc = BFunc String [BytecodeStmt]

data BytecodeValue = LLIString -- Used for primitive call to printf
              | Rdi | Rsi | Rax -- Used for primitives PutChar and PutNum, do not use for anything else
              | Rbx | Rcx  -- General purpose
              | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -- General purpose
              | Rdx -- Used as temporary register to avoid memory to memory actions
              | Rbp | Rsp -- Reserved for stack management
              | Mem BytecodeValue String Int-- A memory address in the stack
              | Const Int -- A constant value, treat as a register so it's polimorfic
              deriving (Eq, Show)

-- Primitive function calls, separate from rest for better pattern matching
data BytecodePrimitive = CCPutChar | CCPrintf | CCExit deriving (Eq, Show)

data BytecodeStmt = Mov BytecodeValue BytecodeValue
                  | MovQ BytecodeValue BytecodeValue
                  | Call String
                  | CCall BytecodePrimitive
                  | Ret
                  | Push BytecodeValue
                  | Pop BytecodeValue
                  | Add BytecodeValue BytecodeValue
                  | Sub BytecodeValue BytecodeValue
                  | Mul BytecodeValue
                  | And BytecodeValue BytecodeValue
                  | Or BytecodeValue BytecodeValue
                  | Not BytecodeValue
                  | Cmp BytecodeValue BytecodeValue
                  | Jmp String
                  | Je String
                  | Jne String
                  | Jle String
                  | Jge String
                  | Jl String
                  | Jg String
                  | Inc BytecodeValue
                  | Sal BytecodeValue BytecodeValue
                  | Label String
                  | Comment String
                  | Commented String BytecodeStmt
                  | EmptyLine
                  deriving (Eq, Show)

--------------------------------------------------------------------------------
--                               HELPERS                                      --
--------------------------------------------------------------------------------
initiallyAvailableRegisters :: [BytecodeValue]
initiallyAvailableRegisters = [Rbx, Rcx, R8, R9, R10, R11, R12, R13, R14, R15]

initiallyUsedRegisters :: [BytecodeValue]
initiallyUsedRegisters = [Rdi, Rdx]

word :: Int -> Int
word n = 8 * n

label :: Int -> String -> String
label i str = str ++ "_" ++ (show i)

--------------------------------------------------------------------------------
--                             INFIX OPERATIONS                               --
--------------------------------------------------------------------------------
infixr 9 +=
(+=) :: BytecodeStmt -> BytecodeStmt -> [BytecodeStmt]
(+=) a b = [a, b]

infixr 9 ++=
(++=) :: BytecodeStmt -> [BytecodeStmt] -> [BytecodeStmt]
(++=) a b = a:b

infixr 9 +==
(+==) :: [BytecodeStmt] -> BytecodeStmt -> [BytecodeStmt]
(+==) a b = a ++ [b]

--------------------------------------------------------------------------------
--                               STATE TYPES                                  --
--------------------------------------------------------------------------------
data FunctionPrologue = FunctionPrologue {
      name                  :: String,
      localVariables        :: [String],
      parameters            :: [String],
      maxExpressionNesting  :: Int
}

data RegisterStatus = RegisterStatus {
  availableRegisters  :: [BytecodeValue],
  usedRegisters       :: [BytecodeValue],
  nextMemoryRegister  :: Int,
  currentFuncPrologue :: Maybe FunctionPrologue,
  nextLabelIdx        :: Int
}

--------------------------------------------------------------------------------
--                               INITIAL STATE                                --
--------------------------------------------------------------------------------
initialRegisterStatus :: RegisterStatus
initialRegisterStatus = RegisterStatus {
  availableRegisters  = initiallyAvailableRegisters,
  usedRegisters       = initiallyUsedRegisters,
  nextMemoryRegister  = 0,
  currentFuncPrologue = Nothing,
  nextLabelIdx        = 0
}

--------------------------------------------------------------------------------
--                         STATE LISTS HELPERS                                --
--------------------------------------------------------------------------------
withIndices :: [a] -> [(Int, a)]
withIndices ls = zip ([0..(length ls)]) ls

mapS :: (a -> State RegisterStatus b) -> [a] -> State RegisterStatus [b]
mapS f [] = do return []
mapS f (x:xs) = do
  y <- f x
  ys <- mapS f xs
  return (y:ys)

concatS :: State RegisterStatus [[a]] -> State RegisterStatus [a]
concatS ls = do
  xs <- ls
  return (concat xs)

--------------------------------------------------------------------------------
--                         STATE TYPES HELPERS                                --
--------------------------------------------------------------------------------
getPrologue :: State RegisterStatus (Maybe FunctionPrologue)
getPrologue = do
  status <- get
  let pr = currentFuncPrologue status
  return pr

isParam :: String -> (State RegisterStatus Bool)
isParam name = do
  pf <- getPrologue
  case pf of
    Nothing -> return False
    Just x -> return (elemIndex name (parameters x) /= Nothing)

isVar :: String -> (State RegisterStatus Bool)
isVar name = do
  pf <- getPrologue
  case pf of
    Nothing -> return False
    Just x -> return (elemIndex name (localVariables x) /= Nothing)

paramIndex :: String -> (State RegisterStatus Int)
paramIndex name = do
  Just pf <- getPrologue
  let Just x = elemIndex name (parameters pf)
  return (word $ x+2)

varIndex :: String -> (State RegisterStatus Int)
varIndex name = do
  Just pf <- getPrologue
  let Just x = elemIndex name (localVariables pf)
  return (word $ x+1)

nextRegister :: State RegisterStatus BytecodeValue
nextRegister = do
  status <- get
  let regs = availableRegisters status
  let used = usedRegisters status
  let nextMem = nextMemoryRegister status
  case regs of
    (x:xs) -> do
      put status {availableRegisters = xs,
                  usedRegisters = x:used }
      return (x)
    otherwise -> do
      put status { nextMemoryRegister = nextMem + 1 }
      return (Mem Rbp "-" (word $ nextMem))

freeRegister :: BytecodeValue -> State RegisterStatus ()
freeRegister reg = do
  status <- get
  let regs = availableRegisters status
  let used = usedRegisters status
  let nextMem = nextMemoryRegister status
  case reg of
    Mem _ _ _ -> do
      put status {nextMemoryRegister = nextMem-1}
    otherwise -> do
      put status {availableRegisters = reg:regs, usedRegisters = delete reg used }

getUsedRegisters :: State RegisterStatus [BytecodeValue]
getUsedRegisters = do
  status <- get
  let used = usedRegisters status
  return used

nextLabelIndex :: State RegisterStatus Int
nextLabelIndex = do
  status <- get
  let i = nextLabelIdx status
  put status { nextLabelIdx = i + 1 }
  return i

localVarsSize :: State RegisterStatus Int
localVarsSize = do
  status <- get
  Just pf <- getPrologue
  let regSize = length $ availableRegisters status
  let varNeeded = length $ localVariables pf
  let nesting = maxExpressionNesting pf
  let exprNeeded = varNeeded + (if regSize - nesting >= 0 then 0 else nesting - regSize)
  return (word $ exprNeeded )

paramsSize :: State RegisterStatus Int
paramsSize = do
  Just pf <- getPrologue
  return (word $ length $ parameters pf)

--------------------------------------------------------------------------------
--                     FUNCTION PROLOGUE CALCULATION                          --
--------------------------------------------------------------------------------
prologue :: FunctionT -> State RegisterStatus ()
prologue (Function name _ parameters block) = do
  let params = map (\(Parameter name _) -> name) parameters
  current <- get
  let (vars, maxExpSize) = variablesInBlock params block
  put current { currentFuncPrologue = Just FunctionPrologue {
      name = name,
      parameters = params,
      localVariables = vars,
      maxExpressionNesting = maxExpSize
    },
    nextMemoryRegister = length vars + 1
  }

variablesInBlock :: [Id] -> BlockT -> ([String], Int)
variablesInBlock p s = variablesInBlock' 0 p s

variablesInBlock' :: Int -> [Id] -> BlockT -> ([String], Int)
variablesInBlock' currentMax params (Block statements) =
  foldr (\s (ac, m) ->
    let (ls, n) = variablesInStatement params s
    in (ac ++ ls, max m n)) ([], currentMax) statements

variablesInStatement :: [Id] -> StmtT -> ([String], Int)
variablesInStatement params (StmtAssign name expr) =
  (if elem name params then [] else [name], calcExprSize expr)
variablesInStatement params (StmtVecAssign name expr expr2) =
  (if elem name params then [] else [name], max (calcExprSize expr) (calcExprSize expr2))
variablesInStatement params (StmtIf expr block) =
  variablesInBlock' (calcExprSize expr) params block
variablesInStatement params (StmtIfElse expr block1 block2) =
  let (vars1, max1) = variablesInBlock' (calcExprSize expr) params block1
  in let (vars2, max2) = variablesInBlock' (calcExprSize expr) params block2
  in ( nub $ vars1 ++ vars2, max max1 max2)
variablesInStatement params (StmtWhile expr block) =
  variablesInBlock' (calcExprSize expr) params block
variablesInStatement _ (StmtReturn expr) = ([], calcExprSize expr)
variablesInStatement _ (StmtCall _ exprs) =
  let mapped = map calcExprSize exprs
  in case mapped of
    [] -> ([], 0)
    otherwise -> ([], maximum mapped)

calcExprSize :: ExprT -> Int
calcExprSize (ExprConstNum _)   = 1
calcExprSize (ExprConstBool _)  = 1
calcExprSize (ExprVecLength _)  = 1
calcExprSize (ExprVar _)        = 1
calcExprSize (ExprVecMake [])   = 0
calcExprSize (ExprCall _ [])    = 0
calcExprSize (ExprNot e)        = calcExprSize e
calcExprSize (ExprVecDeref _ e) = calcExprSize e
calcExprSize (ExprAnd e1 e2)    = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprOr e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprGe e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprLe e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprLt e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprGt e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprEq e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprNe e1 e2)     = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprAdd e1 e2)    = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprSub e1 e2)    = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprMul e1 e2)    = calcExprSize e1 + calcExprSize e2
calcExprSize (ExprCall _ es)    = maximum( map calcExprSize es )
calcExprSize (ExprVecMake es)   = maximum( map calcExprSize es )
