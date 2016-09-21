import Lexer
import Parser
import ASTPrinter

main :: IO ()
main = do
  result <- mapM compareFile filenames
  result <- compareFile (head $ drop 2 filenames)
  putStrLn ""
  putStrLn (show result)

compareFile :: String -> IO Bool
compareFile file = do
  inputed <- readInput file
  expected <- readExpected file
  let ast = parse (tokenize inputed)
  let astPrint = show ast
  putStrLn "------ Print AST"
  putStrLn astPrint
  putStrLn "---------------------"
  putStrLn "------ Print EXPECTED"
  putStrLn expected
  putStrLn "---------------------"
  putStrLn "------ DIFF"
  mapM_ (\(b, x, y) -> 
        do
            putStrLn "-------------------"
            putStrLn "error on "
            putStrLn $ "inputed: \"" ++ x ++ "\""
            putStrLn $ "expected:\"" ++ x ++ "\""
            putStrLn "-------------------"
    ) $ filter (\(b, _, _) -> not b) $ 
            zipWith (\x y -> (x == y, x, y)) (lines astPrint) (lines expected)
  putStrLn "---------------------"
  return (expected == astPrint)

readInput :: String -> IO String
readInput file = readFile (input file)

readExpected :: String -> IO String
readExpected file = readFile (expected file)

input :: String -> String
input file = testdir ++ file ++ ".input"

expected :: String -> String
expected file = testdir ++ file ++ ".expected"

testdir :: String
testdir = "test/inputs/"

filenames :: [String]
filenames = map (\x -> "test" ++ (if x >= 10 then "" else "0") ++ show x) [0..10]