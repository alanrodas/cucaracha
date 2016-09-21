import Test.Hspec

import qualified SpecTypeChecker as TC
import qualified SpecInputFiles as IF

main :: IO ()
main = do
  _ <- TC.test
  _ <- IF.test
  putStrLn "\n2 test executed, check the console to see the results"
