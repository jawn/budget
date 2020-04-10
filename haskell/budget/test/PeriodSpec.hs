module PeriodSpec
    where

import Test.Hspec
import Period
import Data.Time.Calendar

theDay = fromGregorian

spec = do
    describe "Period" $ do
        it "can be created with two days" $ do
            let p = period (theDay 2020 4 1) (theDay 2020 5 31)
            p `shouldBe` (theDay 2020 4 1,theDay 2020 5 31)

        it "can be created with two days in reverse order" $ do
            let p = period (theDay 2020 5 31) (theDay 2020 4 1)
            p `shouldBe` (theDay 2020 4 1,theDay 2020 5 31)

        it "can be created with a year and a month" $ do
            periodFromMonth 2020 04 `shouldBe` (theDay 2020 4 1, theDay 2020 4 30)
            periodFromMonth 2020 02 `shouldBe` (theDay 2020 2 1, theDay 2020 2 29)

        it "can be created with two date strings" $ do
            periodFromStrings "04/01/2020" "05/31/2020" `shouldBe`
                Right (theDay 2020 04 01, theDay 2020 05 31)
            


