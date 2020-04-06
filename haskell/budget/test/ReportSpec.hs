module ReportSpec
    where
import Test.Hspec
import Expense
import Report
import Data.Dates

day1 = date 2020 04 01
day2 = date 2020 04 02
day3 = date 2020 04 13
spec = do
    describe "report" $ do
        it "show the total for a category" $ do
            let exp1 = Expense day1 "Online Services" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let expenses = [exp1, exp2]
            head (report expenses)  `shouldBe` "Online Services                                  :     98.07"

        it "show the total for two categories" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            take 2 (report expenses)  `shouldBe` ["Online Services                                  :     92.00"
                                                 ,"Special                                          :     48.07"]
        it "show the grand total, mentionning the min and max date" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            report expenses  `shouldBe` ["Online Services                                  :     92.00"
                                        ,"Special                                          :     48.07"
                                        ,"TOTAL from 04/01/2020 to 04/13/2020              :    140.07"]

        it "can select categories from a list of categories, for the period of the whole csv file" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            let cats = ["Online Services"]
            reportForCategories cats expenses  `shouldBe` ["Online Services                                  :     92.00"
                                                          ,"TOTAL from 04/01/2020 to 04/13/2020              :     92.00"]


    describe "pretty amount" $ do
        it "print zero on ten positions" $ do
            prettyAmount 0  `shouldBe` "      0.00"
        it "print decimals on correct positions" $ do
            prettyAmount 42 `shouldBe` "      0.42"
        it "print correct positive amounts" $ do
            prettyAmount 4807 `shouldBe` "     48.07"
        it "print correct negative amounts" $ do
            prettyAmount (-4807) `shouldBe` "    -48.07"
