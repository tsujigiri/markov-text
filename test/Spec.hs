import Test.Hspec
import MarkovText
import System.Random

main = hspec $ do
    describe "One way sentence" $ do
        it "returns the same sentence" $ do
            rng <- newStdGen
            take (length sentence) (generate rng [sentence]) `shouldBe` sentence
            where sentence = "The quick brown fox jumps over the lazy dog."
