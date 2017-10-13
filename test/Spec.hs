import Test.Hspec

import qualified P1To10Spec
import qualified P11To20Spec
import qualified P21To30Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problems 1 to 10" P1To10Spec.spec
  describe "Problems 11 to 20" P11To20Spec.spec
  describe "Problems 21 to 30" P21To30Spec.spec
