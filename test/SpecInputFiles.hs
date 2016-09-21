module SpecInputFiles(test) where

import Test.Hspec

import Lexer
import Parser
import Printer
import TypeChecker

pparsed input = show (parsed input)

test :: IO ()
test = hspec $ do
  describe "The parser" $ do
    it "can parse input00" $ do
      input <- readFile "test/inputs/test00.input"
      expected <- readFile "test/inputs/test00.expected"
      pparsed input `shouldBe` expected

    it "can parse input01" $ do
      input <- readFile "test/inputs/test01.input"
      expected <- readFile "test/inputs/test01.expected"
      pparsed input `shouldBe` expected

    it "can parse input02" $ do
      input <- readFile "test/inputs/test02.input"
      expected <- readFile "test/inputs/test02.expected"
      pparsed input `shouldBe` expected

    it "can parse input03" $ do
      input <- readFile "test/inputs/test03.input"
      expected <- readFile "test/inputs/test03.expected"
      pparsed input `shouldBe` expected

    it "can parse input04" $ do
      input <- readFile "test/inputs/test04.input"
      expected <- readFile "test/inputs/test04.expected"
      pparsed input `shouldBe` expected

    it "can parse input05" $ do
      input <- readFile "test/inputs/test05.input"
      expected <- readFile "test/inputs/test05.expected"
      pparsed input `shouldBe` expected

    it "can parse input06" $ do
      input <- readFile "test/inputs/test06.input"
      expected <- readFile "test/inputs/test06.expected"
      pparsed input `shouldBe` expected

    it "can parse input07" $ do
      input <- readFile "test/inputs/test07.input"
      expected <- readFile "test/inputs/test07.expected"
      pparsed input `shouldBe` expected

    it "can parse input08" $ do
      input <- readFile "test/inputs/test08.input"
      expected <- readFile "test/inputs/test08.expected"
      pparsed input `shouldBe` expected

    it "can parse input09" $ do
      input <- readFile "test/inputs/test09.input"
      expected <- readFile "test/inputs/test09.expected"
      pparsed input `shouldBe` expected

    it "can parse input10" $ do
      input <- readFile "test/inputs/test10.input"
      expected <- readFile "test/inputs/test10.expected"
      pparsed input `shouldBe` expected

  describe "The typechecker" $ do
    it "success typechecking input00" $ do
      input <- readFile "test/inputs/test00.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input01" $ do
      input <- readFile "test/inputs/test01.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input02" $ do
      input <- readFile "test/inputs/test02.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input03" $ do
      input <- readFile "test/inputs/test03.input"
      typechecks (parsed input) `shouldBe` True

    it "fail typechecking input04" $ do
      input <- readFile "test/inputs/test04.input"
      typechecks (parsed input) `shouldBe` False

    it "success typechecking input05" $ do
      input <- readFile "test/inputs/test05.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input06" $ do
      input <- readFile "test/inputs/test06.input"
      typechecks (parsed input) `shouldBe` True

    it "fail typechecking input07" $ do
      input <- readFile "test/inputs/test07.input"
      typechecks (parsed input) `shouldBe` False

    it "success typechecking input08" $ do
      input <- readFile "test/inputs/test08.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input09" $ do
      input <- readFile "test/inputs/test09.input"
      typechecks (parsed input) `shouldBe` True

    it "success typechecking input10" $ do
      input <- readFile "test/inputs/test10.input"
      typechecks (parsed input) `shouldBe` True
