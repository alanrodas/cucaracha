module TypeChecker where

import Data.List
import Parser
import Control.Monad

type VarEnv = [(Id,Type)]

type FunctionEnv = [FunctionT]

type TypeResult = Either String Type

raise = Left
success = Right

checkProgram :: ProgramT -> TypeResult
checkProgram EmptyProgram = success Unit
checkProgram (Program fs) = do
    if not (hasMain fs)
       then raise "program has not main" 
       else if not (noRepeatedFunctions fs)
               then raise "there is more than one function with the same id"
               else undefined -- msum $ fmap (checkFunction fs) fs

noRepeatedFunctions :: FunctionEnv -> Bool
noRepeatedFunctions fs = length (nubBy (\f1 f2 -> functionName f1 == functionName f2) fs) == length fs

functionName :: FunctionT -> Id
functionName (Function fname _ _ _) = fname

getFunctionById :: Id -> FunctionEnv -> Maybe FunctionT
getFunctionById fId fs = find ((== fId) . functionName) fs

hasMain :: FunctionEnv -> Bool
hasMain = any isMain

isMain :: FunctionT -> Bool
isMain (Function fname _ _ _) = fname == "main"

checkFunction :: FunctionEnv -> FunctionT -> TypeResult
checkFunction fs (Function fId fT fParams fBlock) = undefined

checkExp :: VarEnv -> ExprT -> TypeResult
checkExp _ _ = success Int

checkBlock :: VarEnv -> BlockT -> TypeResult
checkBlock _ _ = undefined

checkStmt :: VarEnv -> StmtT -> TypeResult
checkStmt _ _ = undefined

