module Main where

import System.Environment

import Lexer
import Parser
import Printer
import TypeChecker

main :: IO ()
main = do
    args <- getArgs
    let
      file = head args
    contents <-readFile file
    let ast = parse (tokenize contents)
    putStrLn (show ast)

    -- imprimimos el resultado de chequear tipos
    -- a drede, independientemente de si tipa o no
    putStrLn ""
    putStrLn "------------ Type errors: "
    let typeCheckerMsg =
            either id (const "the program typechecks")
                   (typecheck ast)
    putStrLn typeCheckerMsg
