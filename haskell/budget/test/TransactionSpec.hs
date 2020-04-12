module TransactionSpec
    where
import Test.Hspec
import Amount
import Account
import Category
import Period
import Transaction
import Data.List
import Data.Time.Calendar

simplified :: Integer -> Int -> Int -> String -> Double -> Transaction
simplified y m d c a = Transaction { transactionAccount = Account "MyBank"
                                   , transactionDate = fromGregorian y m d
                                   , transactionNotes = Just "some notes"
                                   , transactionName = Just "a name"
                                   , transactionCategory = Category c
                                   , transactionAmount = mkAmount a }
mkDate = fromGregorian

spec = do
    describe "Transaction" $ do
        it "has a date, category and amount" $ do
            let t = simplified 2020 4 5 "Training" 48.07
            transactionDate     t `shouldBe` (mkDate 2020 4 5)
            transactionCategory t `shouldBe` (Category "Training")
            transactionAmount   t `shouldBe` (mkAmount 48.07)

        it "also has an account, notes, and a name" $ do
            let t = Transaction (Account "MyBank") (mkDate 2020 4 5) (Just "some notes") (Just "a name") (Category "Groceries") (mkAmount 48.07)
            transactionAccount  t `shouldBe` Account "MyBank"
            transactionName t  `shouldBe` Just "a name"
            transactionNotes t `shouldBe` Just "some notes"

    describe "a list of Transactions" $ do
        it "can be totaled" $ do
            let ts = [ simplified 2020 4 5 "Training" 48.07 
                     , simplified 2020 4 7 "Food" 42.17 
                     , simplified 2020 4 9 "Business Expenses" 1000.00 ]
            totalTransactions ts `shouldBe` mkAmount 1090.24

        describe "can have an average" $ do
            it "for a period of one months" $ do
                let ts = [ simplified 2020 4 5 "Training" 48.07 
                         , simplified 2020 4 7 "Training" 42.17 
                         , simplified 2020 4 9 "Training" 1000.00 ]
                summarizeTransactionsMonths 1 ts `shouldBe` 
                    (Category "Training",mkAmount 1090.24, mkAmount 1090.24)
            it "for a two month period " $ do
                let ts = [ simplified 2020 4 1 "Training" 48.07 
                         , simplified 2020 4 7 "Training" 42.17 
                         , simplified 2020 5 1 "Training" 1000.00 ]
                summarizeTransactionsMonths 2 ts `shouldBe` 
                    (Category "Training",mkAmount 1090.24, mkAmount 545.12)

        it "can determine a period" $ do
            let ts = [ simplified 2020 4 31 "Training" 48.07 
                     , simplified 2020 4 7  "Training" 42.17 
                     , simplified 2020 3 4  "Training" 1000.00 ]
            transactionsPeriod ts `shouldBe` Period (mkDate 2020 3 4) (mkDate 2020 4 31)

    describe "check not empty" $ do
        it "yields a message is transaction list is empty" $ do
            let ts = [simplified 2020 4 5 "Training" 48.07] 
            checkNotEmpty ts `shouldBe` Right ts
            checkNotEmpty [] `shouldBe` Left "no transaction" 


