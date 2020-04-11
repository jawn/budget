module PeriodSpec
    where

import Test.Hspec
import Period
import Data.Time.Calendar

spec = do
    describe "Period" $ do
        it "can be created with two days" $ do
            let p = period (theDay 2020 4 1) (theDay 2020 5 31)
            p `shouldBe` (Period (theDay 2020 4 1) (theDay 2020 5 31))

        it "can be created with two days in reverse order" $ do
            let p = period (theDay 2020 5 31) (theDay 2020 4 1)
            p `shouldBe` Period (theDay 2020 4 1) (theDay 2020 5 31)

        it "can be created with a year and a month" $ do
            periodFromMonth 2020 04 `shouldBe` (Period (theDay 2020 4 1) (theDay 2020 4 30)) 
            periodFromMonth 2020 02 `shouldBe` (Period (theDay 2020 2 1) (theDay 2020 2 29))

        it "can be created with two date strings" $ do
            periodFromStrings "04/01/2020" "05/31/2020" `shouldBe`
                Right (Period (theDay 2020 04 01) (theDay 2020 05 31))

        it "can be shown" $ do
            let p = period (theDay 2020 01 01) (theDay 2020 12 31)
            show p `shouldBe` "from 01/01/2020 to 12/31/2020"

        it "can include a date" $ do
            let p = period (theDay 2020 04 01) (theDay 2020 04 30)
            (theDay 2020 03 31) `within` p `shouldBe` False
            (theDay 2020 04 01) `within` p `shouldBe` True
            (theDay 2020 04 30) `within` p `shouldBe` True
            (theDay 2020 05 01) `within` p `shouldBe` False

            


