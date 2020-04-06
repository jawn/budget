module ReportSpec
    where
import Test.Hspec
import Expense
import Report
import Data.Dates

day1 = mkDate 2020 04 01
day2 = mkDate 2020 04 02
day3 = mkDate 2020 04 13
spec = do
    describe "report" $ do
        it "show the total for a category" $ do
            let exp1 = Expense day1 "Online Services" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let expenses = [exp1, exp2]
            head (reportAllCategories expenses)  `shouldBe` "Online Services                                  :     98.07"

        it "show the total for two categories" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            take 2 (reportAllCategories expenses)  `shouldBe` ["Online Services                                  :     92.00"
                                                 ,"Special                                          :     48.07"]
        it "show the grand total, mentionning the min and max date" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            reportAllCategories expenses  `shouldBe` ["Online Services                                  :     92.00"
                                        ,"Special                                          :     48.07"
                                        ,"TOTAL from 04/01/2020 to 04/13/2020              :    140.07"]

        it "can select categories from a list of categories, for the period of the whole csv file" $ do
            let exp1 = Expense day1 "Special" 4807 
            let exp2 = Expense day2 "Online Services" 5000 
            let exp3 = Expense day3 "Online Services" 4200
            let expenses = [exp1, exp2, exp3]
            let cats = ["Online Services"]
            reportForCategories (`elem` cats) expenses  `shouldBe` ["Online Services                                  :     92.00"
                                                              ,"TOTAL from 04/01/2020 to 04/13/2020              :     92.00"]

    describe "report title" $ do
        it "show a header for the given filename and period" $ do
            let begin = mkDate 2020 01 01
                end = mkDate 2020 03 31
            reportTitle "Bank.csv" Nothing (begin,end) `shouldBe` 
                "Report for file:Bank.csv (all categories) from 01/01/2020 to 03/31/2020"

        it "show a header for the given filename and category list and period" $ do
            let begin = mkDate 2020 01 01
                end = mkDate 2020 03 31
            reportTitle "Bank.csv" (Just "categs.csv") (begin,end) `shouldBe` 
                "Report for file:Bank.csv (categs.csv) from 01/01/2020 to 03/31/2020"

