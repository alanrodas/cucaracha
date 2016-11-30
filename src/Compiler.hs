module Compiler where
import System.Directory
import System.IO
import System.Process

import Precompiler
import PrecompilerPrinter

outputFile :: String -> String -> IO()
outputFile filename datum = do
  appendFile filename ""
  writeFile filename datum

compile bytecode = do
  let filename = "./cucaTest.asm"
  outputFile filename (show bytecode)
  (exit_code, console_out, console_err) <- readProcessWithExitCode "./cuca" [filename] ""
  return console_out
