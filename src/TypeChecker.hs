module TypeChecker where

import Data.List
import Parser
import Control.Monad.Except
import Data.Either
import Data.Function
import Data.Maybe

type VarEnv = [(Id,Type)]

emptyVarEnv = []

addParam :: Id -> Type -> VarEnv -> Result VarEnv
addParam v t vs = return $ (v, t) : vs

addVar :: Id -> Id -> Type -> VarEnv -> Result VarEnv
addVar fId v t vs = 
   maybe (return $ (v, t) : vs)
         (\vT -> fmap (const vs) $ typeInVarEnv fId v t vT)
         (findVar v vs)

findVar :: Id -> VarEnv -> Maybe Type
findVar v vs   = lookup v vs

typeInVarEnv :: Id -> Id -> Type -> Type -> Result ()
typeInVarEnv fId v vType vt = 
   when (vt /= vType) $
      throwError $ "var " ++ v 
                   ++ " in " ++ fId
                   ++ " has type " ++ show vt 
                   ++ " and not " ++ show vType

type FunctionEnv = [FunctionT]

type Result = Except String

primitives :: [FunctionT]
primitives = 
    [
      Function "putChar" Unit [Parameter "x" Int] (Block [])
    , Function "putNum"  Unit [Parameter "x" Int] (Block [])
    ]

checkProgram :: ProgramT -> Result Type
checkProgram EmptyProgram = return Unit
checkProgram (Program fs) = do
    do 
       checkMain fs
       noRepeatedFunctions fs
       noVecFunctions fs
       mapM_ (checkFunction (primitives ++ fs)) fs
       return Unit

noRepeatedFunctions :: FunctionEnv -> Result ()
noRepeatedFunctions fs = 
    when (length (nubBy (on (==) functionName) fs) /= length fs) $
        throwError "there are repeated functions"

noVecFunctions :: FunctionEnv -> Result ()
noVecFunctions fs =
    mapM_ 
      (\(Function fId fT _ _) -> expectedAnyOfF fId nonVecTypes fT)
      fs


functionName :: FunctionT -> Id
functionName (Function fname _ _ _) = fname

getFunctionById :: Id -> FunctionEnv -> Maybe FunctionT
getFunctionById fId fs = 
    find ((== fId) . functionName) fs

getFunctionType :: Id -> FunctionEnv -> Maybe Type
getFunctionType fId fs =
    fmap (\(Function _ t _ _) -> t) (getFunctionById fId fs)

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

expectedT :: String -> Id -> Type -> Type -> Result Type
expectedT msg fId t1 t2 = 
    do
      when (t1 /= t2) $
          throwError $ "type error " ++ msg ++ " in " ++ fId 
                         ++ " found " ++ show t2 
                         ++ " expected " ++ show t1
      return t2

expectedAnyOf :: String -> Id -> [Type] -> Type -> Result Type
expectedAnyOf msg fId ts t2 =
    do
      when (all (/= t2) ts) $
          throwError $ "type error " ++ msg ++ " in " ++ fId 
                         ++ " cannot have type " ++ show t2 
                         ++ " expected " ++ show ts
      return t2


expectedF          = expectedT "function type"
expectedReturn     = expectedT ""
expectedVar    vId = expectedT $ "of var " ++ vId
expectedExp        = expectedT $ "in exp"

expectedAnyOfF     = expectedAnyOf "function type"

types        = [minBound .. maxBound]
nonVecTypes  = filter (/= Vec) types
nonUnitTypes = filter (/= Unit) types

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

checkFunction :: FunctionEnv -> FunctionT -> Result Type
checkFunction fs (Function fId fT fParams fBlock) = 
    do
      paramEnv <- collectParamEnv fId fParams
      varEnv   <- collectVarEnv fId fs paramEnv fBlock
      checkReturn fT fBlock   
      checkBlock fId fs varEnv fBlock

checkReturn :: Type -> BlockT -> Result ()
checkReturn _ _ = return ()

collectParamEnv :: Id -> [ParameterT] -> Result VarEnv
collectParamEnv fId ps = foldM go [] ps
    where go :: VarEnv -> ParameterT -> Result VarEnv
          go r (Parameter pId pType) = do
                     maybe (addParam pId pType r)
                           (\_ -> throwError $ "repeated param " ++ pId
                              ++ " in function " ++ fId)
                           (findVar pId r)

collectVarEnv :: Id -> FunctionEnv ->  VarEnv -> BlockT -> Result VarEnv
collectVarEnv fId fEnv pEnv (Block stmts) = 
    foldM (collectVarStmt fId fEnv) pEnv stmts

collectVarStmt :: Id -> FunctionEnv -> VarEnv -> StmtT -> Result VarEnv
collectVarStmt fId fEnv vEnv (StmtAssign vId exp) = 
    do
      tExp <- checkExp fId fEnv vEnv exp
      addVar fId vId tExp vEnv

collectVarStmt fId fEnv vEnv (StmtIf exp block) = 
    collectVarEnv fId fEnv vEnv block

collectVarStmt fId fEnv vEnv (StmtIfElse exp block1 block2) =
    do
      v1 <- collectVarEnv fId fEnv vEnv block1
      v2 <- collectVarEnv fId fEnv v1 block2
      return v2

collectVarStmt fId fEnv vEnv (StmtWhile exp block) = 
    collectVarEnv fId fEnv vEnv block

collectVarStmt fId fEnv vEnv _ = return vEnv

checkBlock :: Id -> FunctionEnv -> VarEnv -> BlockT -> Result Type
checkBlock fId fenv venv (Block stms) = 
    foldM go Unit stms
    where go :: Type -> StmtT -> Result Type
          go r smt = checkStmt fId fenv venv smt >> return r

checkStmt :: Id -> FunctionEnv -> VarEnv -> StmtT -> Result Type
checkStmt fId fEnv vEnv (StmtAssign vId exp) = return Unit

checkStmt fId fEnv vEnv (StmtVecAssign vId exp1 exp2) = undefined

checkStmt fId fEnv vEnv (StmtIf exp block) = 
    do tExp <- checkExp fId fEnv vEnv exp
       expectedExp fId Bool tExp
       checkBlock fId fEnv vEnv block

checkStmt fId fEnv vEnv (StmtIfElse exp block1 block2) =
    do tExp <- checkExp fId fEnv vEnv exp
       expectedExp fId Bool tExp
       checkBlock fId fEnv vEnv block1
       checkBlock fId fEnv vEnv block2

checkStmt fId fEnv vEnv (StmtWhile exp block) = 
    do tExp <- checkExp fId fEnv vEnv exp
       expectedExp fId Bool tExp
       checkBlock fId fEnv vEnv block

checkStmt fId fEnv vEnv (StmtReturn exp) = 
    do tExp <- checkExp fId fEnv vEnv exp
       let tF = fromJust $ getFunctionType fId fEnv
       expectedExp fId tF tExp

checkStmt fId fEnv vEnv (StmtCall callId exps) = undefined

checkExp :: Id -> FunctionEnv -> VarEnv -> ExprT -> Result Type
checkExp _ _ _ _ = return Int

tests = 
    mapM_ (\(n, p) ->
        putStrLn (n ++ ":") >>
        (putStrLn . show $ runExcept . checkProgram $ p) >>
        putStrLn ""
    )
    [
      ("Empty Program", exampleEmpty)
    , ("Has main", exampleHasMain)
    , ("Hasnt main", exampleHasntMain)
    , ("Main with params", exampleMainParams)
    , ("Main with return", exampleMainWithReturn)
    , ("Main not procedure", exampleMainNotProcedure)
    , ("Repeated functions", exampleRepeatedFunctions)
    , ("Repeated param", exampleRepeatedParam)
    , ("Function not return vec", exampleFunctionVec)
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
    Program [Function "main" Unit [] (Block [
        StmtReturn (ExprConstBool True)
    ])]

exampleMainNotProcedure =
    Program [Function "main" Int [] (Block [])]

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

exampleFunctionVec =
    Program [
        Function "f" Vec [] (Block []),
        Function "main" Unit [] (Block [])
    ]

eParams = 
    [
      Parameter "x" Int
    , Parameter "y" Bool
    , Parameter "z" Vec
    ]

eProc = 
      Function "p" Unit [
          Parameter "x" Int
        , Parameter "y" Bool
        , Parameter "z" Vec
      ] (Block [])

exampleProcedure = 
    Program [
        eProc
      , Function "main" Unit [] (Block [])
    ]

