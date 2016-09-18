module TypeCheckerExamples where

import Control.Monad.Except
import Parser
import TypeChecker

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
    , ("Function type Int return Bool", exampleFuncBadReturn)
    , ("Function return is not last statement", exampleFuncNotLastReturn)

    -- Simple Vars
    , ("Var exists", exampleVarExist)
    , ("Var does not exist", exampleVarNotExist)
    , ("Var doesnt match param", exampleParamNotMatchVar)
    , ("Var doesnt match var", exampleVarNotMatchVar)

    -- Vecs
    , ("Vec of Int", exampleVecOfInt)
    , ("Vec not of Int", exampleVecNotOfInt)
    , ("Vec var in length not exist", exampleVecVarLengthNotExist)
    , ("Vec var in length not Vec", exampleVecVarLengthNotVec)
    , ("Vec var in length is OK", exampleVecVarLengthOK)
    , ("Vec var in deref not exist", exampleVecVarDerefNotExist)
    , ("Vec var in deref not Vec", exampleVecVarDerefNotVec)
    , ("Vec index not Int", exampleVecIndexNotInt)
    , ("Vec deref ok", exampleVecAccessOK)
    , ("Vec assign var not exist", exampleVecAssignVarNotExist)
    , ("Vec assign not Vec var", exampleVecAssignNotVecVar)
    , ("Vec assign not Int", exampleVecAssignNotInt)
    , ("Vec assign index not Int", exampleVecAssignIndexNotInt)
    , ("Vec assign OK", exampleVecAssignOK)

    -- Function calls
    , ("Function call not length params", exampleFuncCallNotLength)
    , ("Function call not type params", exampleFuncCallNotTypes)
    , ("Function call is Unit", exampleFuncCallUnit)
    , ("Function call OK", exampleFuncCallOK)

    -- Function calls
    , ("Procedure call not length params", exampleProcCallNotLength)
    , ("Procedure call not type params", exampleProcCallNotTypes)
    , ("Procedure call not Unit", exampleProcCallNotUnit)
    , ("Procedure call OK", exampleProcCallOK)

    -- Binary Expressions
    , ("And incorrect", exampleExpAnd)
    , ("Or incorrect", exampleExpOr)
    , ("Not incorrect", exampleExpNot)
    , ("<= incorrect", exampleExpLE)
    , (">= incorrect", exampleExpGE)
    , ("< incorrect", exampleExpLT)
    , ("> incorrect", exampleExpGT)
    , ("== incorrect", exampleExpEq)
    , ("!= incorrect", exampleExpNeq)
    , ("(+) incorrect", exampleExpSum)
    , ("(-) incorrect", exampleExpMinus)
    , ("(*) incorrect", exampleExpMult)

    -- control structures
    , ("if condition not bool", exampleIFConditionNotBool)
    , ("if bad then body", exampleIFBadThenBody)
    , ("if bad else body", exampleIFBadElseBody)
    , ("if correct", exampleIFOK)

    , ("while condition not bool", exampleWhileConditionNotBool)
    , ("while bad body", exampleWhileBadBody)
    , ("while correct", exampleWhileOK)
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
        Function "f" Bool [] (Block [
              StmtReturn (ExprConstBool True)
            , StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncBadReturn =
    Program [
        Function "f" Int [] (Block [
              StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncNotLastReturn =
    Program [
        Function "f" Int [] (Block [
              StmtReturn (ExprConstNum 1)
            , StmtAssign "x" (ExprConstNum 1)
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

exampleVecVarLengthNotExist =
    Program [
        Function "f" Int [] (Block [
             StmtReturn 
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecVarLengthNotVec =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v" (ExprConstBool True)
            , StmtReturn 
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecVarLengthOK = 
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2])
            , StmtReturn 
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecVarDerefNotExist =
    Program [
        Function "f" Int [] (Block [
             StmtReturn 
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecVarDerefNotVec =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v" (ExprConstBool True)
            , StmtReturn 
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecIndexNotInt =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtReturn 
                (ExprVecDeref "v" (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAccessOK =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtReturn 
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAssignVarNotExist =
    Program [
        Function "f" Unit [] (Block [
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAssignNotVecVar =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v" (ExprConstNum 1),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAssignNotInt =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAssignIndexNotInt =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstBool True) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleVecAssignOK =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v" 
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncCallNotLength =
    Program [
        Function "f" Int [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
             StmtReturn 
                (ExprConstNum 1)
          ]),
        Function "g" Int [] 
          (Block [
             StmtReturn 
                (ExprCall "f" [
                    ExprConstNum 1
                ])
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncCallNotTypes =
    Program [
        Function "f" Int [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
             StmtReturn 
                (ExprConstNum 1)
          ]),
        Function "g" Int [] 
          (Block [
             StmtReturn 
                (ExprCall "f" [
                     ExprConstNum 1
                   , ExprConstNum 1
                ])
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncCallUnit =
    Program [
        Function "f" Unit [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
          ]),
        Function "g" Int [] 
          (Block [
             StmtReturn 
                (ExprCall "f" [
                     ExprConstNum 1
                   , ExprConstBool True
                ])
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleFuncCallOK =
    Program [
        Function "f" Int [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
             StmtReturn 
                (ExprConstNum 1)
          ]),
        Function "g" Int [] 
          (Block [
             StmtReturn 
                (ExprCall "f" [
                     ExprConstNum 1
                   , ExprConstBool True
                ])
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleProcCallNotLength =
    Program [
        Function "f" Unit [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
          ]),
        Function "g" Unit [] 
          (Block [
            StmtCall "f" [
                ExprConstNum 1
            ]
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleProcCallNotTypes =
    Program [
        Function "f" Unit [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
          ]),
        Function "g" Unit [] 
          (Block [
            StmtCall "f" [
                 ExprConstNum 1
               , ExprConstNum 1
            ]
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleProcCallNotUnit =
    Program [
        Function "f" Int [Parameter "x" Int, Parameter "y" Bool] 
          (Block [
             StmtReturn 
                (ExprConstNum 1)
          ]),
        Function "g" Unit [] 
          (Block [              
            StmtCall "f" [
                 ExprConstNum 1
               , ExprConstBool True
            ]
          ]),
        Function "main" Unit [] (Block [])
    ]

exampleProcCallOK =
    Program [
        Function "f" Unit [Parameter "x" Int, Parameter "y" Bool] 
          (Block [

          ]),
        Function "g" Unit [] 
          (Block [
            StmtCall "f" [
                 ExprConstNum 1
               , ExprConstBool True
            ]
          ]),
        Function "main" Unit [] (Block [])
    ]


exampleExpAnd =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprAnd (ExprConstNum 1) (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpOr =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprOr (ExprConstNum 1) (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpNot =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprNot (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpLE =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprLe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpGE =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprGe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpLT =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprLt (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpGT =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprGt (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpEq =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprEq (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpNeq =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprNe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpSum =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprAdd (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpMinus =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprSub (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleExpMult =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x" 
                (ExprMul (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleIFConditionNotBool =
    Program [
        Function "f" Unit [] (Block [
             StmtIf (ExprConstNum 1) (Block [
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleIFBadThenBody =
    Program [
        Function "f" Unit [] (Block [
             StmtIf (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstNum 1))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleIFBadElseBody =
    Program [
        Function "f" Unit [] (Block [
             StmtIfElse (ExprConstBool True) 
             (Block [
             ])
             (Block [
                StmtAssign "x" (ExprNot (ExprConstNum 1))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleIFOK =
    Program [
        Function "f" Unit [] (Block [
             StmtIfElse (ExprConstBool True) 
             (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
             (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleWhileConditionNotBool = 
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstNum 1) (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleWhileBadBody =
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstNum 1))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

exampleWhileOK =
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

--exampleProcedure = 
--    Program [
--      Function "p" Unit [
--          Parameter "x" Int
--        , Parameter "y" Bool
--        , Parameter "z" Vec
--      ] (Block [
--          StmtAssign "x"  (ExprConstNum 1)
--        , StmtAssign "y"  (ExprConstBool True)
--      ])
--    , Function "main" Unit [] (Block [])
--    ]


--eParams = 
--    [
--      Parameter "x" Int
--    , Parameter "y" Bool
--    , Parameter "z" Vec
--    ]

--eProc = 
--      Function "p" Unit [
--          Parameter "x" Int
--        , Parameter "y" Bool
--        , Parameter "z" Vec
--      ] (Block [
--          StmtAssign "x"  (ExprConstNum 1)
--        , StmtAssign "y"  (ExprConstBool True)
--        , StmtAssign "z"  (ExprVecMake [ExprConstNum 1])
--        , StmtAssign "w"  (ExprConstNum 1)
--        , StmtAssign "j"  (ExprVar "x")
--        , StmtAssign "xs" (ExprVecMake [ExprVar "x", ExprVar "j"])
--      ])

--eFEnv = 
--    [
--      eProc
--    , Function "main" Unit [] (Block [])
--    ]

--exampleCollectVarEnv = 
--    do
--        env1 <- collectParamEnv "f" eParams
--        return $ collectVarEnv "f" 
--                    eFEnv 
--                    env1
--                    (getBlock eProc)