module Main where
import System.Environment
import Lexer
import Parser
import TypeChecker
import Control.Monad.Except
import ASTPrinter

main :: IO ()
main = do
    args <- getArgs
    let
      file = head args
    contents <-readFile file
    let ast = parse (tokenize contents)
    let typeCheckerMsg = 
            either id (const "Correct types") 
                   (runExcept $ checkProgram ast)
    putStrLn typeCheckerMsg
    putStrLn (show ast)