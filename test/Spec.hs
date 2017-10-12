import Test.Hspec

import qualified P1To10Spec
import qualified P11To20Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problems 1 to 10" P1To10Spec.spec
  describe "Problems 11 to 20" P11To20Spec.spec
