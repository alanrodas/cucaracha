main :: IO ()
main = do
  result <- compareFile (head (tail filenames))
  putStrLn result

compareFile file = do
  inputed <- readInput file
  expected <- readExpected file
  result <- expected == inputed
  return result

readInput file = readFile (input file)

readExpected file = readFile (expected file)

input file = testdir ++ file ++ ".input"

expected file = testdir ++ file ++ ".expected"

testdir = "test/inputs/"

filenames = map (\x -> "test" ++ (if x > 10 then '1' else '0') : show x) [0..10]
