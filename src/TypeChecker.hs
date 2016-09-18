module TypeChecker where

import Data.List
import Parser
import Control.Monad.Except
import Data.Either
import Data.Function

type VarEnv = [(Id,Type)]

emptyVarEnv = []
addVar  v vs = v : vs
findVar v vs = lookup v vs

type FunctionEnv = [FunctionT]

type Result      = Except String
type TypeResult  = Result Type

checkProgram :: ProgramT -> TypeResult
checkProgram EmptyProgram = return Unit
checkProgram (Program fs) = do
    do 
       checkMain fs
       noRepeatedFunctions fs
       mapM_ (checkFunction fs) fs
       return Unit

noRepeatedFunctions :: FunctionEnv -> Result ()
noRepeatedFunctions fs = 
    when (length (nubBy (on (==) functionName) fs) /= length fs) $
        throwError "there are repeated functions"

functionName :: FunctionT -> Id
functionName (Function fname _ _ _) = fname

getFunctionById :: Id -> FunctionEnv -> Maybe FunctionT
getFunctionById fId fs = 
    find ((== fId) . functionName) fs

checkMain :: FunctionEnv -> Result ()
checkMain fs = case getFunctionById "main" fs of 
    Nothing -> 
        throwError "non empty program has not a main function"
    Just (Function "main" fT fParams fBlock) -> 
        do
          when (not . null $ fParams) $
              throwError "main cannot have params"
          expectedF "main" Unit fT
          noReturn "main" fBlock

expectedT :: String -> Id -> Type -> Type -> TypeResult
expectedT msg fId t1 t2 = 
    do
      when (t1 /= t2) $
          throwError $ "type error " ++ msg ++ " in " ++ fId 
                         ++ " found " ++ show t2 
                         ++ " expected " ++ show t1
      return t1

expectedF          = expectedT "function type"
expectedReturn     = expectedT ""
expectedVar    vId = expectedT $ "of var " ++ vId

noReturn :: Id -> BlockT -> Result ()
noReturn fId (Block cmds) = 
    when (not . null $ filter isReturn cmds) $
        throwError $ "function " ++ fId ++ " cannot have return"

oneReturn :: Id -> BlockT -> Result ()
oneReturn fId (Block [])   = return () 
oneReturn fId (Block cmds) = 
    do
      when (length (filter isReturn cmds) > 1) $
          throwError $ fId ++ " function cannot have more than one return"
      when (not $ isReturn (last cmds)) $
          throwError $ "the last cmd of "
          ++ fId 
          ++ " must be a return"

isReturn :: StmtT -> Bool
isReturn (StmtReturn _) = True
isReturn _ = False

checkFunction :: FunctionEnv -> FunctionT -> TypeResult
checkFunction fs (Function fId fT   fParams fBlock) = 
    do
      paramEnv <- collectParamEnv fId fParams
      checkBlock fId [] fBlock

collectParamEnv :: Id -> [ParameterT] -> Except String VarEnv
collectParamEnv fId ps = foldM go [] ps
    where go :: VarEnv -> ParameterT -> Except String VarEnv
          go r (Parameter pId pType) = do
                     maybe (return $ addVar (pId, pType) r)
                           (\_ -> throwError $ "repeated param " ++ pId
                              ++ " in function " ++ fId)
                           (findVar pId r)

checkExp :: Id -> VarEnv -> ExprT -> TypeResult
checkExp _ _ _ = return Int

checkBlock :: Id -> VarEnv -> BlockT -> TypeResult
checkBlock _ _ _ = return Int

checkStmt :: Id -> VarEnv -> StmtT -> TypeResult
checkStmt _ _ _  = return Unit

tests = 
    mapM_ (\(n, p) ->
        putStrLn . show $ 
        (n, runExcept . checkProgram $ p)
    )
    [
      ("Empty", exampleEmpty)
    , ("Has main", exampleHasMain)
    , ("Hasnt main", exampleHasntMain)
    , ("Main with params", exampleMainParams)
    , ("Main with return", exampleMainWithReturn)
    , ("Main not procedure", exampleMainNotProcedure)
    , ("Repeated functions", exampleRepeatedFunctions)
    , ("Repeated param", exampleRepeatedParam)
    ]

exampleEmpty = 
    EmptyProgram

exampleHasMain = 
    Program [Function "main" Unit [] (Block [])]

exampleHasntMain = 
    Program [Function "f" Unit [] (Block [])]

exampleMainParams =
    Program [Function "main" Unit [(Parameter "x" Int)] (Block [])]

exampleMainWithReturn =
    Program [Function "main" Int [] (Block [])]

exampleMainNotProcedure =
    Program [Function "main" Unit [] (Block [
        StmtReturn (ExprConstBool True)
    ])]

exampleRepeatedFunctions = 
    Program [
        Function "f" Unit [] (Block []),
        Function "f" Unit [] (Block []),
        Function "main" Unit [] (Block [])
    ]

exampleRepeatedParam = 
    Program [
      Function "f" Unit [
          Parameter "x" Int
        , Parameter "x" Int
      ] (Block [])
      , Function "main" Unit [] (Block [])
    ]

exampleProcedure = 
    Program [
      Function "p" Unit [
          Parameter "x" Int
        , Parameter "y" Bool
        , Parameter "z" Vec
      ] (Block [])
      , Function "main" Unit [] (Block [])
    ]

