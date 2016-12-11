{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad(when)
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process

import qualified Lexer
import qualified Parser
import qualified Printer
import qualified TypeChecker
import qualified Assembler
import qualified AssemblyPrinter
import qualified Compiler

data Cucaracha = Cucaracha {
                     tokens   :: Bool
                    ,ast      :: Bool
                    ,check    :: Bool
                    ,assembly :: Bool
                    ,compile  :: Bool
                    ,execute  :: Bool
                    ,file     :: String
                    ,out      :: String
                 }
              deriving (Show, Data, Typeable)

cucarachaConsole = Cucaracha{
             tokens    = def &= help "Output the tokenization string to the console"
            ,ast       = def &= help "Output the AST string to the console" &= name "a"
            ,check     = def &= help "Output the typechecking status of the code to the console"
            ,assembly  = def &= help "Produce only the assembly file but do not compile it" &= name "e"
            ,compile   = def &= help "Produce the compiled file using GCC" &= name "C"
            ,execute   = def &= help "Execute the outputed compiled file" &= name "X"
            ,file      = def &= help "The input file" &= typ "[input]" &= opt "program.cuca"
            ,out       = def &= help "The output file" &= typ "[output]" &= opt "program"
         }

main :: IO()
main = do
    args <- (cmdArgs $ cucarachaConsole
                     &= help "The cucaracha language parser and compiler"
                     &= program "Cucaracha"
                     &= summary "Cucaracha v1.0.0.0"
            )
    dir <- getCurrentDirectory
    inputFile <- (canonicalizePath (file args))

    existsInput <- doesFileExist (inputFile)
    -- Exit if the input file does not exists
    when (not existsInput)  (die ("The given input file \"" ++ (file args) ++ "\"does not exist"))

    contents <-readFile inputFile

    -- tokenize
    let tokenized = Lexer.tokenize contents
    -- print results if demanded by the user
    when (tokens args) (putStrLn (show tokenized))
    -- Exit if there are no more stept to perform to increment performance
    when (tokens args && not (ast args) && not (check args) && not (assembly args) && not (compile args || execute args)) (exitSuccess)

    -- parse
    let asted = Parser.parse tokenized
    -- print results if demanded by the user
    when (ast args) (putStrLn (show asted))
    -- Exit if there are no more stept to perform to increment performance
    when (ast args && not (check args) && not (assembly args) && not (compile args || execute args)) (exitSuccess)

    -- typecheck
    let tc = TypeChecker.typechecks asted
    -- print results if demanded by the user
    -- also print the result if there are typecheck error prior to exiting
    when (check args && tc) (putStrLn "The program has passed type check validation")
    when (not tc) (putStrLn ("Compilation error: " ++ (TypeChecker.typecheckErrors asted)))
    -- Exit if there are no more stept to perform to increment performance
    -- Also exit if the program did not typecheck correctly
    when (not tc || (check args && not (assembly args) && not (compile args || execute args))) (exitSuccess)

    -- assembly
    let assembled = Assembler.assemble asted
    let filename = if  (assembly args && not (compile args || execute args)) then (out args) else (out args) ++ ".asm"
    Compiler.saveAssembly filename assembled
    -- Exit if there are no more stept to perform to increment performance
    when (assembly args && not (compile args || execute args)) (exitSuccess)

    -- fully compile
    compileStatus <- Compiler.compileFile filename (out args)
    -- Delete intermediate assembly if it was not requested
    when (not (assembly args)) (Compiler.deleteFile filename)

    -- Print compilation message at this point
    when (compileStatus /= "") (putStrLn compileStatus)
    when (compileStatus == "") (putStrLn "Cucaracha program compiled succesfully")

    -- Exit if there are no more stept to perform to increment performance
    when (not (execute args)) (exitSuccess)

    -- If the program is going to be ran, print a waring
    when (execute args) (putStrLn "Running the generated program:")
    when (execute args) (putStrLn "")
    when (execute args) (putStrLn "-------------------------------")

    -- execute the program if it was requested so
    runResult <- Compiler.run (out args)
    putStrLn runResult

    -- print program execution finished message
    putStrLn "-------------------------------"
    putStrLn ""
    putStrLn "Cucaracha execution finished"
