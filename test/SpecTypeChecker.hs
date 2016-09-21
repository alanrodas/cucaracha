module SpecTypeChecker(test) where

import Test.Hspec

import Lexer
import Parser
import Printer
import TypeChecker

test :: IO ()
test = hspec $ do
  describe "The typechecker should type a program that" $ do
    it "is empty" $ do
      (typechecks programEmpty) `shouldBe` True

    it "has an empty main" $ do
      (typechecks programWithEmptyMain) `shouldBe` True

    it "dereferences an item over a vector with a int index" $ do
      (typechecks expDerefOnVectorWithIntIndex) `shouldBe` True

    it "assigns to a vector item an integer value to vector variable with integer index" $ do
      (typechecks expVecAssignWithVectorAndIntIndexWithIntValue) `shouldBe` True

    it "calls a function as an expression with correct number of arguments and types" $ do
      (typechecks expFuncCallWithCorrectArgs) `shouldBe` True

    it "calls a function as a statement with correct number of arguments and types" $ do
      (typechecks stmtFuncCallWithCorrectArgs) `shouldBe` True


  describe "The typechecker should type a program where" $ do
    it "variables are declared before usage" $ do
      (typechecks varIsDeclared) `shouldBe` True

    it "variables are assigned a vector of integers" $ do
      (typechecks varVectorOfInts) `shouldBe` True

    it "length is asked to a vector" $ do
      (typechecks expLengthOfVector) `shouldBe` True

    it "an if statement fullfills typechecking in both blocks and condition is boolean" $ do
      (typechecks stmtIfCorrect) `shouldBe` True

    it "a while statement fullfills typechecking i it's block and condition is boolean" $ do
      (typechecks stmtWhileCorrect) `shouldBe` True


  describe "The typechecker shouldn't type a program that" $ do
    it "derreferences an item of non declared variable" $ do
      (typechecks expDerefOnNonDeclared) `shouldBe` False

    it "dereferences an item of a non vector variable" $ do
      (typechecks expDerefOnNonVector) `shouldBe` False

    it "dereferences an item of a vector with non int index" $ do
      (typechecks expDerefOnNonIntIndex) `shouldBe` False

    it "assigns to a vector item on a non declared variable" $ do
      (typechecks exprVecAssignOnNonDeclared) `shouldBe` False

    it "assigns to a vector item on a non vector variable" $ do
      (typechecks exprVecAssignOnNonVector) `shouldBe` False

    it "assigns to a vector item on vector with value other than integer" $ do
      (typechecks exprVecAssignWithNonIntValue) `shouldBe` False

    it "assigns to a vector item on a vector with a non integer index" $ do
      (typechecks exprVecAssignWithNonIntIndex) `shouldBe` False

    it "calls a function as an expression with different number of argument that parameters declared" $ do
      (typechecks expFuncCallWrongArgsNum) `shouldBe` False

    it "calls a function as an expression with argument that do not match declared parameter types" $ do
      (typechecks expFuncCallWrongTypes) `shouldBe` False

    it "calls a function that has unit type as an expression" $ do
      (typechecks expFunCallOfUnitType) `shouldBe` False

    it "calls a function as a statement with different number of argument that parameters declared" $ do
      (typechecks stmtFuncCallWrongArgsNum) `shouldBe` False

    it "calls a function as a statement with argument that do not match declared parameter types" $ do
      (typechecks stmtFuncCallWrongTypes) `shouldBe` False

    it "calls a function that hasn't got unit type as a statement" $ do
      (typechecks stmtFunCallOfNonUnitType) `shouldBe` False

    it "uses and in non bool values" $ do
      (typechecks expAndOnNonBools) `shouldBe` False

    it "uses or in non bool values" $ do
      (typechecks expOrOnNonBools) `shouldBe` False

    it "uses not in non bool values" $ do
      (typechecks expNotOnNonBool) `shouldBe` False

    it "uses <= in non integer values" $ do
      (typechecks expLeOnNonNumbers) `shouldBe` False

    it "uses >= in non integer values" $ do
      (typechecks expGeOnNonNumbers) `shouldBe` False

    it "uses < in non integer values" $ do
      (typechecks expLtOnNonNumbers) `shouldBe` False

    it "uses > in non integer values" $ do
      (typechecks expGtOnNonNumbers) `shouldBe` False

    it "uses == in non integer values" $ do
      (typechecks expEqOnNonNumbers) `shouldBe` False

    it "uses != in non integer values" $ do
      (typechecks expNeOnNonNumbers) `shouldBe` False

    it "uses + in non integer values" $ do
      (typechecks expGtOnNonNumbers) `shouldBe` False

    it "uses - in non integer values" $ do
      (typechecks expEqOnNonNumbers) `shouldBe` False

    it "uses * in non integer values" $ do
      (typechecks expNeOnNonNumbers) `shouldBe` False


  describe "The typechecker shouldn't type a program where" $ do
    it "main declaration exists" $ do
      (typechecks programWithoutMain) `shouldBe` False

    it "main declaration has parameters" $ do
      (typechecks programWithMainWithParams) `shouldBe` False

    it "main declaration has return statement" $ do
      (typechecks programWithMainWithReturn) `shouldBe` False

    it "main declaration has type other than unit" $ do
      (typechecks programWithMainNotUnitType) `shouldBe` False

    it "are repeated declarations of functions" $ do
      (typechecks programWithRepeatedDeclaration) `shouldBe` False

    it "variables are not declared before usage" $ do
      (typechecks varIsNotDeclared) `shouldBe` False

    it "variables are assigned a parameter with a different type" $ do
      (typechecks varTypeNotMatchParamTypeInAssign) `shouldBe` False

    it "variables are assigned a variable with a different type" $ do
      (typechecks varTypeNotMatchVarTypeInAssign) `shouldBe` False

    it "variables are assigned a vector of something different than integers" $ do
      (typechecks varVectorNotOfInts) `shouldBe` False

    it "length is asked to a non declared variable" $ do
      (typechecks expLengthOfNonDeclared) `shouldBe` False

    it "length is asked to a non vector variable" $ do
      (typechecks expLengthOfNonVector) `shouldBe` False

    it "an if has a non boolean condition" $ do
      (typechecks smtsIfNonBoolCondition) `shouldBe` False

    it "an if has a then block does that not fulfill typechecking" $ do
      (typechecks smtsIfIncorrectBodyThen) `shouldBe` False

    it "an if has an else block does that not fulfill typechecking" $ do
      (typechecks smtsIfIncorrectBodyElse) `shouldBe` False

    it "a while has a non boolean condition" $ do
      (typechecks stmtWhileNonBooleanCondition) `shouldBe` False

    it "a while has a block does that not fulfill typechecking" $ do
      (typechecks stmtWhileBadBody) `shouldBe` False


  describe "The typechecker shouldn't type a program with functions that" $ do
    it "have repeated param names" $ do
      (typechecks functionWithRepeatedParams) `shouldBe` False

    it "return vectors" $ do
      (typechecks funcReturnsVec) `shouldBe` False

    it "has unit type but has a return statement" $ do
      (typechecks funcUnitTypeWithReturn) `shouldBe` False

    it "doesn't have unit type but hasn't got a return statement" $ do
      (typechecks funcNotUnitTypeWithoutReturn) `shouldBe` False

    it "have more than one return statement" $ do
      (typechecks funcWithTwoReturns) `shouldBe` False

    it "has return's a type other that the declared function type" $ do
      (typechecks funcReturnTypeOtherThanDeclared) `shouldBe` False

    it "hasn't got the return statement as it's last line" $ do
      (typechecks funcReturnNotLastLine) `shouldBe` False




-- programs
programEmpty =
    EmptyProgram

programWithEmptyMain =
    Program [Function "main" Unit [] (Block [])]

programWithoutMain =
    Program [Function "f" Unit [] (Block [])]

programWithMainWithParams =
    Program [Function "main" Unit [(Parameter "x" Int)] (Block [])]

programWithMainWithReturn =
    Program [Function "main" Unit [] (Block [
        StmtReturn (ExprConstBool True)
    ])]

programWithMainNotUnitType =
    Program [Function "main" Int [] (Block [])]

programWithRepeatedDeclaration =
    Program [
        Function "f" Unit [] (Block []),
        Function "f" Unit [] (Block []),
        Function "main" Unit [] (Block [])
    ]

-- functions
functionWithRepeatedParams =
    Program [
      Function "f" Unit [
          Parameter "x" Int
        , Parameter "x" Int
      ] (Block [])
      , Function "main" Unit [] (Block [])
    ]

funcReturnsVec =
    Program [
        Function "f" Vec [] (Block []),
        Function "main" Unit [] (Block [])
    ]

funcUnitTypeWithReturn =
    Program [
        Function "f" Unit [] (Block [
            StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

funcNotUnitTypeWithoutReturn =
    Program [
        Function "f" Int [] (Block []),
        Function "main" Unit [] (Block [])
    ]

funcWithTwoReturns =
    Program [
        Function "f" Bool [] (Block [
              StmtReturn (ExprConstBool True)
            , StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

funcReturnTypeOtherThanDeclared =
    Program [
        Function "f" Int [] (Block [
              StmtReturn (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

funcReturnNotLastLine =
    Program [
        Function "f" Int [] (Block [
              StmtReturn (ExprConstNum 1)
            , StmtAssign "x" (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

-- variables
varIsDeclared =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "x" (ExprConstNum 1)
            , StmtReturn (ExprVar "x")
        ]),
        Function "main" Unit [] (Block [])
    ]

varIsNotDeclared =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "x" (ExprConstNum 1)
            , StmtReturn (ExprVar "w")
        ]),
        Function "main" Unit [] (Block [])
    ]

varTypeNotMatchParamTypeInAssign =
    Program [
        Function "f" Int [Parameter "x" Bool] (Block [
              StmtAssign "y" (ExprConstNum 1)
            , StmtAssign "y" (ExprVar "x")
            , StmtReturn (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

varTypeNotMatchVarTypeInAssign =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "x" (ExprConstBool True)
            , StmtAssign "y" (ExprConstNum 1)
            , StmtAssign "y" (ExprVar "x")
            , StmtReturn (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

varVectorOfInts =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2])
        ]),
        Function "main" Unit [] (Block [])
    ]

varVectorNotOfInts =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprVecMake [ExprConstNum 1, ExprConstBool True])
        ]),
        Function "main" Unit [] (Block [])
    ]

-- expressions
expLengthOfNonDeclared =
    Program [
        Function "f" Int [] (Block [
             StmtReturn
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

expLengthOfNonVector =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v" (ExprConstBool True)
            , StmtReturn
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

expLengthOfVector =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2])
            , StmtReturn
                (ExprVecLength "v")
        ]),
        Function "main" Unit [] (Block [])
    ]

expDerefOnNonDeclared =
    Program [
        Function "f" Int [] (Block [
             StmtReturn
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

expDerefOnNonVector =
    Program [
        Function "f" Int [] (Block [
              StmtAssign "v" (ExprConstBool True)
            , StmtReturn
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

expDerefOnNonIntIndex =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtReturn
                (ExprVecDeref "v" (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expDerefOnVectorWithIntIndex =
    Program [
        Function "f" Int [] (Block [
             StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtReturn
                (ExprVecDeref "v" (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

exprVecAssignOnNonDeclared =
    Program [
        Function "f" Unit [] (Block [
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exprVecAssignOnNonVector =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v" (ExprConstNum 1),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

exprVecAssignWithNonIntValue =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstBool True)
        ]),
        Function "main" Unit [] (Block [])
    ]

exprVecAssignWithNonIntIndex =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstBool True) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

expVecAssignWithVectorAndIntIndexWithIntValue =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "v"
                (ExprVecMake [ExprConstNum 1, ExprConstNum 2]),
             StmtVecAssign "v" (ExprConstNum 1) (ExprConstNum 1)
        ]),
        Function "main" Unit [] (Block [])
    ]

expFuncCallWrongArgsNum =
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

expFuncCallWrongTypes =
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

expFunCallOfUnitType =
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

expFuncCallWithCorrectArgs =
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

-- comparison and boolean expresions

expAndOnNonBools =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprAnd (ExprConstNum 1) (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

expOrOnNonBools =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprOr (ExprConstNum 1) (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

expNotOnNonBool =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprNot (ExprConstNum 1))
        ]),
        Function "main" Unit [] (Block [])
    ]

expLeOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprLe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expGeOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprGe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expLtOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprLt (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expGtOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprGt (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expEqOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprEq (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expNeOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprNe (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expAddOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprAdd (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expSubOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprSub (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

expMulOnNonNumbers =
    Program [
        Function "f" Unit [] (Block [
             StmtAssign "x"
                (ExprMul (ExprConstBool True) (ExprConstBool True))
        ]),
        Function "main" Unit [] (Block [])
    ]

-- statements
stmtFuncCallWrongArgsNum =
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

stmtFuncCallWrongTypes =
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

stmtFunCallOfNonUnitType =
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

stmtFuncCallWithCorrectArgs =
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


smtsIfNonBoolCondition =
    Program [
        Function "f" Unit [] (Block [
             StmtIf (ExprConstNum 1) (Block [
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

smtsIfIncorrectBodyThen =
    Program [
        Function "f" Unit [] (Block [
             StmtIf (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstNum 1))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

smtsIfIncorrectBodyElse =
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

stmtIfCorrect =
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

stmtWhileNonBooleanCondition =
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstNum 1) (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

stmtWhileBadBody =
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstNum 1))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]

stmtWhileCorrect =
    Program [
        Function "f" Unit [] (Block [
             StmtWhile (ExprConstBool True) (Block [
                StmtAssign "x" (ExprNot (ExprConstBool True))
             ])
        ]),
        Function "main" Unit [] (Block [])
    ]
