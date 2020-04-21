module AmountSpec
    where
import Amount ( Amount
              , amount
              , divideBy
              , number
              , total
              )

import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Amount" $ do
        it "can be created from a positive float" $ do
            let a = amount 48.07
            show a `shouldBe` "48.07"

        it "can be negated" $ do
            let a = amount 48.07
            negate (negate a)  `shouldBe` a
            (negate a) + a `shouldBe` amount 0

        it "can be created from a negative float" $ do
            let a = amount (-48.07)
            show a `shouldBe` "-48.07"

        it "can be read from a string positive" $ do
            let a = read "42.17" :: Amount
            show a `shouldBe` "42.17"

        it "can be read from a string negative" $ do
            let a = read "-42.17" :: Amount
            show a `shouldBe` "-42.17"

        it "can extract its internal number" $ do
            let a = amount (-123.45)
            number a  `shouldBe` (-12345)

        it "can be divided by an integer" $ do
            let a = amount 1000.00
            show (a `divideBy` 4) `shouldBe` "250.00"

    describe "a list of amounts" $ do
        it "can be totaled" $ do
            let as = map amount [42.17,-30.00,4807.00]
            total as `shouldBe` amount 4819.17
