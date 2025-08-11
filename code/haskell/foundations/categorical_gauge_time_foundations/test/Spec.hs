import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Categorical Gauge Time Foundations" $ do
        it "Basic test framework is set up" $ do
            True `shouldBe` True
        
        it "QuickCheck works" $ property $ \x ->
            (x :: Int) + 0 == x