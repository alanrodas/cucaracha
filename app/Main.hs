module Main where
import System.Environment
import Lexer
import Parser
import TypeChecker
-- import ASTPrinter


main :: IO ()
main = do
    args <- getArgs
    let
      file = head args
    contents <-readFile file
    let ast = parse (tokenize contents)
    if either (const True) (const False) (checkProgram ast)
       then putStrLn (show ast)
       else putStrLn "error de tipos"