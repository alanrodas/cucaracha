module Main where
import System.Environment
import Lexer
import Parser
-- import ASTPrinter


main :: IO ()
main = do
    args <- getArgs
    let
      file = head args
    contents <-readFile file
    let ast = show (parse (tokenize contents))
    putStrLn ast
