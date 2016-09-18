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
         (\vT -> fmap (const vs) $ varHasType fId v t vT)
         (findVar v vs)

findVar :: Id -> VarEnv -> Maybe Type
findVar v vs   = lookup v vs

varHasType :: Id -> Id -> Type -> Type -> Result Type
varHasType fId v vType vt = 
    do
      when (vt /= vType) $
        throwError $ "var " ++ v 
                     ++ " in " ++ fId
                     ++ " has type " ++ show vt 
                     ++ " and not " ++ show vType
      return vType

typeInVarEnv :: Id -> Id -> VarEnv -> Result Type
typeInVarEnv fId v vEnv =
    maybe 
        (throwError $ "var " ++ v 
                       ++ " in " ++ fId
                       ++ " does not exist")
        (return)
        (findVar v vEnv)


getBlock :: FunctionT -> BlockT
getBlock (Function _ _ _ b) = b

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

findFunctionById :: Id -> FunctionEnv -> Result FunctionT
findFunctionById fId fs = 
    maybe 
       (throwError $ "function " ++ fId ++ "does not exist")
       (return)
       (getFunctionById fId fs)

getFunctionType :: Id -> FunctionEnv -> Result Type
getFunctionType fId fs =
    fmap (\(Function _ t _ _) -> t) $ findFunctionById fId fs

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
expectedT msg fId tExpected tFound = 
    do
      when (tExpected /= tFound) $
          throwError $ "type error " ++ msg ++ " in " ++ fId 
                         ++ " found " ++ show tFound 
                         ++ " expected " ++ show tExpected
      return tFound

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

expectedAnyOfF   = expectedAnyOf "function type"
expectedAnyOfCF  = expectedAnyOf "function call" []

types         = [minBound .. maxBound]
functionTypes = [Int, Bool]
nonVecTypes   = filter (/= Vec) types
nonUnitTypes  = filter (/= Unit) types

noReturn :: Id -> BlockT -> Result ()
noReturn fId (Block cmds) = 
    when (not . null . collectReturn $ Block cmds) $
        throwError $ "procedure " ++ fId ++ " cannot have return"

oneReturn :: Id -> BlockT -> Result ()
oneReturn fId (Block cmds) = 
    do
      let rets = collectReturn (Block cmds)
      when (null rets) $
          throwError $ "function " ++ fId ++ 
            " must have a return statement"
      when (length rets > 1) $
          throwError $ "function " ++ fId ++ 
            " cannot have more than one return"
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
      checkReturn fId fT fBlock 
      checkBlock fId fs varEnv fBlock

checkReturn :: Id -> Type -> BlockT -> Result ()
checkReturn fId Unit b = noReturn  fId b
checkReturn fId t    b = oneReturn fId b

collectReturn :: BlockT -> [StmtT]
collectReturn (Block xs) = concatMap collectReturnFromStmt xs

collectReturnFromStmt :: StmtT -> [StmtT]
collectReturnFromStmt (StmtReturn e) =
    [StmtReturn e]

collectReturnFromStmt (StmtIf _ block) = 
    collectReturn block

collectReturnFromStmt (StmtIfElse _ block1 block2) =
    collectReturn block1 ++ collectReturn block2

collectReturnFromStmt (StmtWhile _ block) = 
    collectReturn block

collectReturnFromStmt _ = []

---------------------------------------------------------------
-- COLLECT VAR ENV --------------------------------------------
---------------------------------------------------------------

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

---------------------------------------------------------------
-- CHECK STATEMENTS -------------------------------------------
---------------------------------------------------------------

checkBlock :: Id -> FunctionEnv -> VarEnv -> BlockT -> Result Type
checkBlock fId fenv venv (Block stms) = 
    foldM go Unit stms
    where go :: Type -> StmtT -> Result Type
          go r smt = checkStmt fId fenv venv smt >> return r

checkStmt :: Id -> FunctionEnv -> VarEnv -> StmtT -> Result Type
checkStmt fId fEnv vEnv (StmtAssign vId exp) = 
    do
      t  <- typeInVarEnv fId vId vEnv
      tE <- checkExp fId fEnv vEnv exp
      varHasType fId vId t tE
      return Unit

checkStmt fId fEnv vEnv (StmtVecAssign vId exp1 exp2) = 
    do
      t  <- typeInVarEnv fId vId vEnv
      varHasType fId vId t Vec
      tI <- checkExp fId fEnv vEnv exp1
      tE <- checkExp fId fEnv vEnv exp2
      when (tI /= Int) $
          throwError $ 
               "index of " ++ vId 
            ++ " in function " ++ fId
            ++ " has not type Int"
      when (tE /= Int) $
          throwError $ 
               "assignment to vector " ++ vId 
            ++ " in function " ++ fId
            ++ " must have type Int "
            ++ " cannot accept type " ++ show tE
      return Unit

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
       tF <- getFunctionType fId fEnv
       expectedExp fId tF tExp

checkStmt fId fEnv vEnv (StmtCall cId es) = 
    do
      (Function _ fT ps b) <- findFunctionById cId fEnv
      tEs <- mapM (checkExp fId fEnv vEnv) es
      let tPs = map (\(Parameter _ t) -> t) ps
      when (length tEs /= length tPs) $
        throwError $ 
            "amount of arguments of function call " ++ cId
            ++ " in " ++ fId ++ " does not match params"
      when (tEs /= tPs) $
        throwError $ 
            "types of arguments of function call " ++ cId
            ++ " in " ++ fId ++ " does not match params"
      expectedF cId Unit fT
      return Unit

---------------------------------------------------------------
-- CHECK EXPRESSIONS ------------------------------------------
---------------------------------------------------------------

checkExp :: Id -> FunctionEnv -> VarEnv -> ExprT -> Result Type

checkExp fId fEnv vEnv (ExprVar vId) = 
    typeInVarEnv fId vId vEnv 

checkExp fId fEnv vEnv (ExprConstNum n) = 
    return Int

checkExp fId fEnv vEnv (ExprConstBool b) = 
    return Bool 

checkExp fId fEnv vEnv (ExprVecMake es) = 
    do
      ts <- mapM (checkExp fId fEnv vEnv) es
      when (not $ all (== Int) ts) $
          throwError $ 
              "type error in " ++ fId
              ++ " not all exprs in Vec have type Int"
      return Vec

checkExp fId fEnv vEnv (ExprVecLength vecId) = 
    do
      t <- typeInVarEnv fId vecId vEnv
      varHasType fId vecId t Vec
      return Int

checkExp fId fEnv vEnv (ExprVecDeref vecId e) =
    do
      t  <- typeInVarEnv fId vecId vEnv
      varHasType fId vecId t Vec
      tE <- checkExp fId fEnv vEnv e
      when (tE /= Int) $
          throwError $ 
               "index of " ++ vecId 
            ++ " in function " ++ fId
            ++ " has not type Int"
      return Int

checkExp fId fEnv vEnv (ExprCall cId es) = 
    do
      (Function _ fT ps b) <- findFunctionById cId fEnv
      tEs <- mapM (checkExp fId fEnv vEnv) es
      let tPs = map (\(Parameter _ t) -> t) ps
      when (length tEs /= length tPs) $
        throwError $ 
            "amount of arguments of function call " ++ cId
            ++ " in " ++ fId ++ " does not match params"
      when (tEs /= tPs) $
        throwError $ 
            "types of arguments of function call " ++ cId
            ++ " in " ++ fId ++ " does not match params"
      expectedAnyOfCF functionTypes fT
      return fT

checkExp fId fEnv vEnv (ExprAnd e1 e2) = 
    checkBinOp fId fEnv vEnv Bool e1 e2 Bool

checkExp fId fEnv vEnv (ExprOr e1 e2)  = 
    checkBinOp fId fEnv vEnv Bool e1 e2 Bool

checkExp fId fEnv vEnv (ExprNot e)     = 
    do
      t <- checkExp fId fEnv vEnv e
      expectedExp fId Bool t
      return Bool

checkExp fId fEnv vEnv (ExprLe e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprGe e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprLt e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprGt e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprEq e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprNe e1 e2)  = 
    checkBinOp fId fEnv vEnv Int e1 e2 Bool

checkExp fId fEnv vEnv (ExprAdd e1 e2) = 
    checkBinOp fId fEnv vEnv Int e1 e2 Int

checkExp fId fEnv vEnv (ExprSub e1 e2) = 
    checkBinOp fId fEnv vEnv Int e1 e2 Int

checkExp fId fEnv vEnv (ExprMul e1 e2) = 
    checkBinOp fId fEnv vEnv Int e1 e2 Int

checkBinOp ::
    Id -> FunctionEnv -> VarEnv -> 
    Type -> ExprT -> ExprT -> Type -> Result Type
checkBinOp fId fEnv vEnv tExpected e1 e2 tResult =
    do
      t1 <- checkExp fId fEnv vEnv e1
      t2 <- checkExp fId fEnv vEnv e2
      expectedExp fId tExpected t1
      expectedExp fId tExpected t2
      return tResult

---------------------------------------------------------------
-- CHECK EXAMPLES ---------------------------------------------
---------------------------------------------------------------

tests = 
    mapM_ (\(n, p) ->
        putStrLn (n ++ ":") >>
        (putStrLn . show $ runExcept . checkProgram $ p) >>
        putStrLn ""
    )
    [
    -- check of function env
      ("Empty Program", exampleEmpty)
    , ("Has main", exampleHasMain)
    , ("Hasnt main", exampleHasntMain)
    , ("Main with params", exampleMainParams)
    , ("Main with return", exampleMainWithReturn)
    , ("Main not Unit", exampleMainNotProcedure)
    , ("Repeated functions", exampleRepeatedFunctions)
    , ("Repeated param", exampleRepeatedParam)
    , ("Function cannot return Vec", exampleFunctionVec)
    , ("Procedure with return", exampleProcReturn)
    , ("Function without return", exampleFuncNotReturn)
    , ("Function more than one return", exampleFuncTwoReturn)

    -- expressions
    , ("Var exists", exampleVarExist)
    , ("Var does not exist", exampleVarNotExist)
    , ("Var doesnt match param", exampleParamNotMatchVar)
    , ("Var doesnt match var", exampleVarNotMatchVar)
    , ("Vec of Int", exampleVecOfInt)
    , ("Vec not of Int", exampleVecNotOfInt)
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

exampleProcReturn =
    Program [
        Function "f" Unit [] (Block [
            StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncNotReturn =
    Program [
        Function "f" Int [] (Block []),
        Function "main" Unit [] (Block [])
    ]

exampleFuncTwoReturn =
    Program [
        Function "f" Int [] (Block [
              StmtReturn (ExprConstBool True)
            , StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVarExist =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "x" (ExprConstNum 1)
            , StmtReturn (ExprVar "x")
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVarNotExist =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "x" (ExprConstNum 1)
            , StmtReturn (ExprVar "w")
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleParamNotMatchVar =
    Program [
        Function "f" Int [Parameter "x" Bool] (Block [
              StmtAssign "y" (ExprConstNum 1)
            , StmtAssign "y" (ExprVar "x")
            , StmtReturn (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVarNotMatchVar =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "x" (ExprConstBool True)
            , StmtAssign "y" (ExprConstNum 1)
            , StmtAssign "y" (ExprVar "x")
            , StmtReturn (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecOfInt =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecNotOfInt =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprVecMake [ExprConstNum 1, ExprConstBool True])
        ]),
        Function "main" Unit [] (Block [])
    ]


exampleProcedure = 
    Program [
      Function "p" Unit [
          Parameter "x" Int
        , Parameter "y" Bool
        , Parameter "z" Vec
      ] (Block [
          StmtAssign "x"  (ExprConstNum 1)
        , StmtAssign "y"  (ExprConstBool True)
      ])
    , Function "main" Unit [] (Block [])
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
      ] (Block [
          StmtAssign "x"  (ExprConstNum 1)
        , StmtAssign "y"  (ExprConstBool True)
        , StmtAssign "z"  (ExprVecMake [ExprConstNum 1])
        , StmtAssign "w"  (ExprConstNum 1)
        , StmtAssign "j"  (ExprVar "x")
        , StmtAssign "xs" (ExprVecMake [ExprVar "x", ExprVar "j"])
      ])

eFEnv = 
    [
      eProc
    , Function "main" Unit [] (Block [])
    ]

exampleCollectVarEnv = 
    do
        env1 <- collectParamEnv "f" eParams
        return $ collectVarEnv "f" 
                    eFEnv 
                    env1
                    (getBlock eProc)

exampleComplete = 
    Program [
        eProc
      , Function "main" Unit [] (Block [])
    ]

