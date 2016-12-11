import Test.Hspec

import qualified SpecTypeChecker as TC
import qualified SpecParserInputFiles as PIF
import qualified SpecCompilerInputFiles as CIF

main :: IO ()
main = do
  _ <- TC.test
  _ <- PIF.test
  _ <- CIF.test
  putStrLn "\n3 test groups executed, check the console above to see the results"
