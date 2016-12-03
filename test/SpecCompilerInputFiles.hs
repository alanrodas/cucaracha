module SpecCompilerInputFiles(test) where

import Test.Hspec

import Lexer
import Parser
import Printer
import TypeChecker
import Precompiler
import PrecompilerPrinter
import Compiler

pcompiled input = compile (precompile (parsed input))

test :: IO ()
test = hspec $ do
  describe "The compiler should be able to compile and run" $ do
    it "00 - an empty program" $ do
      input <- readFile "test/inputs/compiler/test00.input"
      expected <- readFile "test/inputs/compiler/test00.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "01 - a putChar with constant number" $ do
      input <- readFile "test/inputs/compiler/test01.input"
      expected <- readFile "test/inputs/compiler/test01.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "02 - a sequence of putChar's with constant numbers" $ do
      input <- readFile "test/inputs/compiler/test02.input"
      expected <- readFile "test/inputs/compiler/test02.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "03 - a putNum in sequence with putChars" $ do
      input <- readFile "test/inputs/compiler/test03.input"
      expected <- readFile "test/inputs/compiler/test03.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "04 - a parameterless procedure call" $ do
      input <- readFile "test/inputs/compiler/test04.input"
      expected <- readFile "test/inputs/compiler/test04.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "05 - nested parameterless procedure calls" $ do
      input <- readFile "test/inputs/compiler/test05.input"
      expected <- readFile "test/inputs/compiler/test05.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "06 - a procedure call with one parameter" $ do
      input <- readFile "test/inputs/compiler/test06.input"
      expected <- readFile "test/inputs/compiler/test06.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "07 - a procedure call with multiple parameters" $ do
      input <- readFile "test/inputs/compiler/test07.input"
      expected <- readFile "test/inputs/compiler/test07.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "08 - a nested procedure call with multiple parameters" $ do
      input <- readFile "test/inputs/compiler/test08.input"
      expected <- readFile "test/inputs/compiler/test08.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "09 - one local variable usage" $ do
      input <- readFile "test/inputs/compiler/test09.input"
      expected <- readFile "test/inputs/compiler/test09.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected
{-
    it "10 - many local variables usage" $ do
      input <- readFile "test/inputs/compiler/test10.input"
      expected <- readFile "test/inputs/compiler/test10.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "11 - many more local variable usage" $ do
      input <- readFile "test/inputs/compiler/test11.input"
      expected <- readFile "test/inputs/compiler/test11.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "12 - local variables plus parameters call" $ do
      input <- readFile "test/inputs/compiler/test12.input"
      expected <- readFile "test/inputs/compiler/test12.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "13 - parameter assignments" $ do
      input <- readFile "test/inputs/compiler/test13.input"
      expected <- readFile "test/inputs/compiler/test13.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "14 - procedures with parameters and local variables combined" $ do
      input <- readFile "test/inputs/compiler/test14.input"
      expected <- readFile "test/inputs/compiler/test14.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "15 - sum expressions with constants and variables" $ do
      input <- readFile "test/inputs/compiler/test15.input"
      expected <- readFile "test/inputs/compiler/test15.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "16 - nested sum expressions" $ do
      input <- readFile "test/inputs/compiler/test16.input"
      expected <- readFile "test/inputs/compiler/test16.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "17 - nested sum expressions using parameters" $ do
      input <- readFile "test/inputs/compiler/test17.input"
      expected <- readFile "test/inputs/compiler/test17.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "18 - nested sum expressions using all registers" $ do
      input <- readFile "test/inputs/compiler/test18.input"
      expected <- readFile "test/inputs/compiler/test18.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected


    it "19 - perform subtraction with constants and variables" $ do
      input <- readFile "test/inputs/compiler/test19.input"
      expected <- readFile "test/inputs/compiler/test19.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "20 - subtraction complex expressions" $ do
      input <- readFile "test/inputs/compiler/test20.input"
      expected <- readFile "test/inputs/compiler/test20.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "21 - sums and subtractions using all registers" $ do
      input <- readFile "test/inputs/compiler/test21.input"
      expected <- readFile "test/inputs/compiler/test21.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "22 - multiplications with constants and variables" $ do
      input <- readFile "test/inputs/compiler/test22.input"
      expected <- readFile "test/inputs/compiler/test22.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "23 - nested multiplications" $ do
      input <- readFile "test/inputs/compiler/test23.input"
      expected <- readFile "test/inputs/compiler/test23.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "24 - multiplying parameters" $ do
      input <- readFile "test/inputs/compiler/test24.input"
      expected <- readFile "test/inputs/compiler/test24.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "25 - Boolean constants and if without else" $ do
      input <- readFile "test/inputs/compiler/test25.input"
      expected <- readFile "test/inputs/compiler/test25.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "26 - If with else and nested ifs" $ do
      input <- readFile "test/inputs/compiler/test26.input"
      expected <- readFile "test/inputs/compiler/test26.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "27 - And expressions" $ do
      input <- readFile "test/inputs/compiler/test27.input"
      expected <- readFile "test/inputs/compiler/test27.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "28 - Or expressions" $ do
      input <- readFile "test/inputs/compiler/test28.input"
      expected <- readFile "test/inputs/compiler/test28.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "29 - Not expressions" $ do
      input <- readFile "test/inputs/compiler/test29.input"
      expected <- readFile "test/inputs/compiler/test29.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "30 - Equality comparisons" $ do
      input <- readFile "test/inputs/compiler/test30.input"
      expected <- readFile "test/inputs/compiler/test30.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "31 - Logical operations ands and ors" $ do
      input <- readFile "test/inputs/compiler/test31.input"
      expected <- readFile "test/inputs/compiler/test31.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "32 - Greater equal" $ do
      input <- readFile "test/inputs/compiler/test32.input"
      expected <- readFile "test/inputs/compiler/test32.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "33 - Less or equal" $ do
      input <- readFile "test/inputs/compiler/test33.input"
      expected <- readFile "test/inputs/compiler/test3.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "34 - Greater stric" $ do
      input <- readFile "test/inputs/compiler/test34.input"
      expected <- readFile "test/inputs/compiler/test34.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "35 - Less strict" $ do
      input <- readFile "test/inputs/compiler/test35.input"
      expected <- readFile "test/inputs/compiler/test35.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "36 - Not equal" $ do
      input <- readFile "test/inputs/compiler/test36.input"
      expected <- readFile "test/inputs/compiler/test36.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "37 - Relational operations" $ do
      input <- readFile "test/inputs/compiler/test37.input"
      expected <- readFile "test/inputs/compiler/test37.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "38 - Functions that return values" $ do
      input <- readFile "test/inputs/compiler/test38.input"
      expected <- readFile "test/inputs/compiler/test38.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "39 - Recursive factorial v1" $ do
      input <- readFile "test/inputs/compiler/test39.input"
      expected <- readFile "test/inputs/compiler/test39.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "40 - Recursive factorial v2" $ do
      input <- readFile "test/inputs/compiler/test40.input"
      expected <- readFile "test/inputs/compiler/test40.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "41 - Div mod recursive" $ do
      input <- readFile "test/inputs/compiler/test41.input"
      expected <- readFile "test/inputs/compiler/test41.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "42 - Odd Even mutually recursive" $ do
      input <- readFile "test/inputs/compiler/test42.input"
      expected <- readFile "test/inputs/compiler/test42.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "43 - Fibonacci recursive" $ do
      input <- readFile "test/inputs/compiler/test43.input"
      expected <- readFile "test/inputs/compiler/test43.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "44 - While, iterative factorial" $ do
      input <- readFile "test/inputs/compiler/test44.input"
      expected <- readFile "test/inputs/compiler/test44.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "45 - While, div mod iterative" $ do
      input <- readFile "test/inputs/compiler/test45.input"
      expected <- readFile "test/inputs/compiler/test45.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "46 - While fibonacci iterative" $ do
      input <- readFile "test/inputs/compiler/test46.input"
      expected <- readFile "test/inputs/compiler/test46.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "47 - Nested whiles" $ do
      input <- readFile "test/inputs/compiler/test47.input"
      expected <- readFile "test/inputs/compiler/test47.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "48 - Local variables order declaration testing" $ do
      input <- readFile "test/inputs/compiler/test48.input"
      expected <- readFile "test/inputs/compiler/test48.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "49 - While with nested if" $ do
      input <- readFile "test/inputs/compiler/test49.input"
      expected <- readFile "test/inputs/compiler/test49.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "50 - While with nested if else" $ do
      input <- readFile "test/inputs/compiler/test50.input"
      expected <- readFile "test/inputs/compiler/test50.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

      it "51 - Vector creations and length testing" $ do
      input <- readFile "test/inputs/compiler/test51.input"
      expected <- readFile "test/inputs/compiler/test51.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "52 - Vector creation length and access" $ do
      input <- readFile "test/inputs/compiler/test52.input"
      expected <- readFile "test/inputs/compiler/test52.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "53 - Vector creation length and access 2" $ do
      input <- readFile "test/inputs/compiler/test53.input"
      expected <- readFile "test/inputs/compiler/test53.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "54 - Vector modify" $ do
      input <- readFile "test/inputs/compiler/test54.input"
      expected <- readFile "test/inputs/compiler/test54.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "55 - Vector modify 2" $ do
      input <- readFile "test/inputs/compiler/test55.input"
      expected <- readFile "test/inputs/compiler/test55.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected
-}
