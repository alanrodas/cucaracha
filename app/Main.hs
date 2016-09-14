module Main where
import System.Environment
import Lexer
import Parser
-- import ASTPrinter
import TypeChecker


main :: IO ()
main = do
    args <- getArgs
    let
      file = head args
    contents <-readFile file
    let ast = parse (tokenize contents)
    if checkProgram ast
       then putStrLn (show ast)
       else putStrLn "error de tipos"