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
  describe "The parser" $ do
    it "can parse input00" $ do
      input <- readFile "test/inputs/compiler/test00.input"
      expected <- readFile "test/inputs/compiler/test00.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input01" $ do
      input <- readFile "test/inputs/compiler/test01.input"
      expected <- readFile "test/inputs/compiler/test01.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input02" $ do
      input <- readFile "test/inputs/compiler/test02.input"
      expected <- readFile "test/inputs/compiler/test02.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input03" $ do
      input <- readFile "test/inputs/compiler/test03.input"
      expected <- readFile "test/inputs/compiler/test03.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input04" $ do
      input <- readFile "test/inputs/compiler/test04.input"
      expected <- readFile "test/inputs/compiler/test04.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

{-
    it "can parse input05" $ do
      input <- readFile "test/inputs/compiler/test05.input"
      expected <- readFile "test/inputs/compiler/test05.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input06" $ do
      input <- readFile "test/inputs/compiler/test06.input"
      expected <- readFile "test/inputs/compiler/test06.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input07" $ do
      input <- readFile "test/inputs/compiler/test07.input"
      expected <- readFile "test/inputs/compiler/test07.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input08" $ do
      input <- readFile "test/inputs/compiler/test08.input"
      expected <- readFile "test/inputs/compiler/test08.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input09" $ do
      input <- readFile "test/inputs/compiler/test09.input"
      expected <- readFile "test/inputs/compiler/test09.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected

    it "can parse input10" $ do
      input <- readFile "test/inputs/compiler/test10.input"
      expected <- readFile "test/inputs/compiler/test10.expected"
      compiled <- pcompiled input
      compiled `shouldBe` expected
-}
