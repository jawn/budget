module TransactionSpec
    where
import Test.Hspec
import Amount
import Category
import Transaction
import Data.List
import Data.Time.Calendar

mkDate = fromGregorian
spec = do
    describe "Transaction" $ do
        it "has a date, category and amount" $ do
            let t = Transaction (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
            transactionDate     t `shouldBe` (mkDate 2020 4 5)
            transactionCategory t `shouldBe` (Category "Training")
            transactionAmount   t `shouldBe` (mkAmount 48.07)

    describe "a list of Transactions" $ do
        it "can be totaled" $ do
            let ts = [Transaction (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
                     ,Transaction (mkDate 2020 4 7) (Category "Food") (mkAmount 42.17)  
                     ,Transaction (mkDate 2020 4 9) (Category "Business Expenses") (mkAmount 1000.00)]
            totalTransactions ts `shouldBe` mkAmount 1090.24

        describe "can have an average" $ do
            it "for a period of one months" $ do
                let ts = [Transaction (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
                         ,Transaction (mkDate 2020 4 7) (Category "Training") (mkAmount 42.17)  
                         ,Transaction (mkDate 2020 4 9) (Category "Training") (mkAmount 1000.00)]
                summarizeTransactionsMonths 1 ts `shouldBe` 
                    (Category "Training",mkAmount 1090.24, mkAmount 1090.24)
            it "for a two month period " $ do
                let ts = [Transaction (mkDate 2020 4 1) (Category "Training") (mkAmount 48.07)
                         ,Transaction (mkDate 2020 4 7) (Category "Training") (mkAmount 42.17)  
                         ,Transaction (mkDate 2020 5 1) (Category "Training") (mkAmount 1000.00)]
                summarizeTransactionsMonths 2 ts `shouldBe` 
                    (Category "Training",mkAmount 1090.24, mkAmount 545.12)

        it "can determine a period" $ do
            let ts = [Transaction (mkDate 2020 4 31) (Category "Training") (mkAmount 48.07)
                     ,Transaction (mkDate 2020 4 7) (Category "Training") (mkAmount 42.17)  
                     ,Transaction (mkDate 2020 3 4) (Category "Training") (mkAmount 1000.00)]
            transactionsPeriod ts `shouldBe` (mkDate 2020 3 4, mkDate 2020 4 31)


                        
