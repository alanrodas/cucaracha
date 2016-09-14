module TypeChecker where

import Parser

type VarEnv  = [(Id,Type)]

checkProgram :: ProgramT -> Bool
checkProgram = undefined

checkFunction :: FunctionT -> Bool
checkFunction = undefined

checkExp :: VarEnv -> ExprT -> Bool
checkExp = undefined

checkBlock :: VarEnv -> BlockT -> Bool
checkBlock = undefined

checkStmt :: VarEnv -> StmtT -> Bool
checkStmt = undefined