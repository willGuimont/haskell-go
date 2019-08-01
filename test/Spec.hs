import           Control.Exception     (evaluate)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe
