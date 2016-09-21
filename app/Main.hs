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
    putStrLn (show ast)

    putStrLn ""
    putStrLn "------------ Type errors: "
    let typeCheckerMsg =
            either id (const "the program typechecks")
                   (runExcept $ checkProgram ast)
    putStrLn typeCheckerMsg
