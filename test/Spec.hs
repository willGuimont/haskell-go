import Control.Exception (evaluate)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import BoardSpec
import BoardUtilsSpec

main :: IO ()
main = hspec $ do
  boardSpec
  boardUtilsSpec