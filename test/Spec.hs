import Test.Hspec

import qualified SpecTypeChecker as TC
import qualified SpecParserInputFiles as PIF
import qualified SpecCompilerInputFiles as CIF

main :: IO ()
main = do
  -- _ <- TC.test
  -- _ <- PIF.test
  _ <- CIF.test
  putStrLn "\n2 test executed, check the console to see the results"
