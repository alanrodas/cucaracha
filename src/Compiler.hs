-- NOTE the compiler only compiles for Linux, as I do not own a windows copy
-- and I was not able to run this in macOS
module Compiler(
  saveAssembly,
  compileFile,
  compile,
  compileAndRun,
  run,
  deleteFile
) where

import System.Directory
import System.Exit
import System.IO
import System.Process

import Assembler
import AssemblyPrinter

saveAssembly :: String -> Bytecode -> IO()
saveAssembly filename bytecode = do
  appendFile filename ""
  writeFile filename (show bytecode)

compileFile :: String -> String -> IO String
compileFile inputFile outputFile = do
  dir <- getCurrentDirectory
  -- nasm -g -felf64 -o $objectfile $filename;
  (exit_code_nasm, console_out_nasm, console_err_nasm) <- readProcessWithExitCode "nasm" ["-felf64", "-o", outputFile ++ ".o", inputFile] ""
  -- gcc -o $output $objectfile;
  (exit_code_gcc, console_out_gcc, console_err_gcc) <- readProcessWithExitCode "gcc" ["-o", outputFile, outputFile ++ ".o"] ""
  --(exit_code, console_out, console_err) <- readProcessWithExitCode "bash" [dir ++ "/cuca", inputFile] ""
  --return (if exit_code == ExitSuccess then console_out else console_err)
  deleteFile (outputFile ++ ".o")
  return (if exit_code_nasm /= ExitSuccess
    then console_err_nasm else if exit_code_gcc /= ExitSuccess
      then console_err_gcc
      else console_out_gcc)

compile :: Bytecode -> String -> IO String
compile bytecode filename = do
  saveAssembly (filename++".asm") bytecode
  result <- compileFile (filename++".asm") (filename)
  return result

compileAndRun :: Bytecode -> String -> IO String
compileAndRun bytecode filename = do
  result <- compile bytecode filename
  if result /= ""
    then return result
    else do
      runned <- run filename
      return runned

run :: String -> IO String
run filename = do
  (exit_code, console_out, console_err) <- readProcessWithExitCode ("./" ++ filename) [] ""
  return (if exit_code /= ExitSuccess
    then console_err
    else console_out)

deleteFile :: String -> IO ()
deleteFile filename = do
  removeFile filename
