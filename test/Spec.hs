-- could only write this line
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- the following is the full way
import Test.Hspec

import qualified P1To10Spec
import qualified P11To20Spec
import qualified P21To28Spec
import qualified P31To41Spec
import qualified P46To50Spec
import qualified P56To60Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Problems 1 to 10" P1To10Spec.spec
  describe "Problems 11 to 20" P11To20Spec.spec
  describe "Problems 21 to 28" P21To28Spec.spec
  describe "Problems 31 to 41" P31To41Spec.spec
  describe "Problems 46 to 50" P46To50Spec.spec
  describe "Problems 56 to 60" P56To60Spec.spec