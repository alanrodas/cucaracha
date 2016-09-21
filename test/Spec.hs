import Lexer
import Parser
import ASTPrinter
import TypeChecker

main :: IO ()
main = do
  result <- mapM compareFile filenames
  putStrLn $ "All tests passed? " ++ show (and result)

compareFile :: String -> IO Bool
compareFile file = do  
  let pathInputed = input file
  let pathExpected = expected file
  putStrLn $ 
    "Comparing " ++ pathInputed ++ " and " ++ pathExpected

  inputed <- readFile pathInputed
  expected <- readFile pathExpected
  let ast = parse (tokenize inputed)
  let astPrint = show ast
  let result = expected == astPrint
  putStrLn $ "Equal content? " ++ show result
  putStrLn $ "Typechecks? " ++ (either
    (\x -> "No! " ++ x)
    (\_ -> "Yes")
    (runTypeCheck ast))
  putStrLn ""
  return result

input :: String -> String
input file = testdir ++ file ++ ".input"

expected :: String -> String
expected file = testdir ++ file ++ ".expected"

testdir :: String
testdir = "test/inputs/"

filenames :: [String]
filenames = map (\x -> "test" ++ (if x >= 10 then "" else "0") ++ show x) [0..10]