
module AmountSpec
    where
import Test.Hspec
import Amount

spec = do
    describe "Amount" $ do
        it "can be created from a positive float" $ do
            let a = mkAmount 48.07
            show a `shouldBe` "48.07"

        it "can be negated" $ do
            let a = mkAmount 48.07
            negate (negate a)  `shouldBe` a
            (negate a) + a `shouldBe` mkAmount 0

        it "can be created from a negative float" $ do
            let a = mkAmount (-48.07)
            show a `shouldBe` "-48.07"

        it "can be read from a string positive" $ do
            let a = read "42.17" :: Amount
            show a `shouldBe` "42.17"

        it "can be read from a string negative" $ do
            let a = read "-42.17" :: Amount
            show a `shouldBe` "-42.17"

        it "can extract its internal number" $ do
            let a = mkAmount (-123.45)
            number a  `shouldBe` (-12345)

    describe "a list of amounts" $ do
        it "can be totaled" $ do
            let as = map mkAmount [42.17,-30.00,4807.00]
            totalAmount as `shouldBe` mkAmount 4819.17