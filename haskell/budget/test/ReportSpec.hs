module ReportSpec
    where
import Test.Hspec
import Transaction
import Category
import Report
import Data.Dates
import Data.Time.Calendar

theDay = fromGregorian
day1 = theDay 2020 04 01
day2 = theDay 2020 04 02
day3 = theDay 2020 04 13
spec = do
    describe "report" $ do
        it "show the total for a category" $ do
            let t1 = Transaction day1 (Category "Online Services") 4807 
            let t2 = Transaction day2 (Category "Online Services") 5000 
            let transactions = [t1, t2]
            take 60 (head (reportAllCategories transactions))
                `shouldBe` "Online Services                                  :     98.07"

        it "show the total for two categories" $ do
            let t1 = Transaction day1 (Category "Special") 4807 
            let t2 = Transaction day2 (Category "Online Services") 5000 
            let t3 = Transaction day3 (Category "Online Services") 4200
            let transactions = [t1, t2, t3]
            map (take 60) (take 2 (reportAllCategories transactions))
            `shouldBe` ["Online Services                                  :     92.00"
                       ,"Special                                          :     48.07"]
        it "show the grand total, mentionning the min and max date" $ do
            let t1 = Transaction day1 (Category "Special") 4807 
            let t2 = Transaction day2 (Category "Online Services") 5000 
            let t3 = Transaction day3 (Category "Online Services") 4200
            let transactions = [t1, t2, t3]
            map (take 60) (reportAllCategories transactions) 
            `shouldBe` ["Online Services                                  :     92.00"
                       ,"Special                                          :     48.07"
                       ,"TOTAL from 04/01/2020 to 04/13/2020              :    140.07"]

        it "can select categories from a list of categories, for the period of the whole csv file" $ do
            let t1 = Transaction day1 (Category "Special") 4807 
            let t2 = Transaction day2 (Category "Online Services") 5000 
            let t3 = Transaction day3 (Category "Online Services") 4200
            let transactions = [t1, t2, t3]
            let cats = [(Category "Online Services")]
            map (take 60) (reportForCategories (`elem` cats) transactions)
            `shouldBe` ["Online Services                                  :     92.00"
                       ,"TOTAL from 04/01/2020 to 04/13/2020              :     92.00"]

    describe "show the average amount of each category according to the number of month in the period" $ do
        it "for a one month period" $ do
            let t1 = Transaction (theDay 2020 4 1) (Category "Online Services") 4807 
            let t2 = Transaction (theDay 2020 4 5) (Category "Online Services") 5000 
            let transactions = [t1, t2]
            head (reportAllCategories transactions) `shouldBe`
                "Online Services                                  :     98.07 |     98.07"
        it "for a two months period" $ do
            let t1 = Transaction (theDay 2020 4 1) (Category "Online Services") 4807 
            let t2 = Transaction (theDay 2020 5 5) (Category "Online Services") 5000 
            let transactions = [t1, t2]
            head (reportAllCategories transactions) `shouldBe` 
                "Online Services                                  :     98.07 |     49.03"
        it "for a two months period in one category" $ do
            let t1 = Transaction (theDay 2020 4 1) (Category "Online Services") 4807 
            let t2 = Transaction (theDay 2020 5 31) (Category "Training") 5000 
            let transactions = [t1, t2]
            head (reportForCategories (== Category "Training") transactions) `shouldBe` 
                "Training                                         :     50.00 |     25.00"
    it "show the average amount for all transactions according to the number of month in the period" $ do
        let t1 = Transaction (theDay 2020 4 1) (Category "Online Services") 4807 
        let t2 = Transaction (theDay 2020 5 31) (Category "Training") 5000 
        let transactions = [t1,t2]
        last (reportAllCategories transactions) `shouldBe` 
                "TOTAL from 04/01/2020 to 05/31/2020              :     98.07 |     49.03"

    describe "report title" $ do
        it "show a header for the given filename and period" $ do
            let begin = theDay 2020 01 01
                end = theDay 2020 03 31
            reportTitle (Just "Bank.csv") Nothing (begin,end) `shouldBe` 
                "Report for file:Bank.csv (all categories) from 01/01/2020 to 03/31/2020"

        it "show a header for the given filename and category list and period" $ do
            let begin = theDay 2020 01 01
                end = theDay 2020 03 31
            reportTitle (Just "Bank.csv") (Just "categs.csv") (begin,end) `shouldBe` 
                "Report for file:Bank.csv (categs.csv) from 01/01/2020 to 03/31/2020"

        it "show a header for the period" $ do
            let begin = theDay 2020 01 01
                end = theDay 2020 03 31
            reportTitle Nothing Nothing (begin,end) `shouldBe` 
                "Report (all categories) from 01/01/2020 to 03/31/2020"

        it "show a header for a selection of categories and the period" $ do
            let begin = theDay 2020 01 01
                end = theDay 2020 03 31
            reportTitle Nothing (Just "categs.csv") (begin,end) `shouldBe` 
                "Report (categs.csv) from 01/01/2020 to 03/31/2020"
