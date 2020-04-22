module TransactionSpec
    where
import Test.Hspec
import Amount
import Name
import Note
import Account
import Category
import Period
import Transaction
import Data.Time.Calendar

simplified :: Integer -> Int -> Int -> String -> Double -> Transaction
simplified y m d c a = Transaction { transactionAccount = Account "MyBank"
                                   , transactionDate = fromGregorian y m d
                                   , transactionNotes = Just $ Note "some notes"
                                   , transactionName = Just $ Name "a name"
                                   , transactionCategory = Category c
                                   , transactionAmount = amount a }

mkDate :: Integer -> Int -> Int -> Day
mkDate = fromGregorian

spec :: SpecWith ()
spec = do
    describe "Transaction" $ do
        it "has a date, category and amount" $ do
            let t = simplified 2020 4 5 "Training" 48.07
            transactionDate     t `shouldBe` (mkDate 2020 4 5)
            transactionCategory t `shouldBe` (Category "Training")
            transactionAmount   t `shouldBe` (amount 48.07)

        it "also has an account, notes, and a name" $ do
            let t = Transaction (Account "MyBank") (mkDate 2020 4 5) (Just $ Note "some notes") (Just $ Name "a name") (Category "Groceries") (amount 48.07)
            transactionAccount  t `shouldBe` Account "MyBank"
            transactionName t  `shouldBe` Just (Name "a name")
            transactionNotes t `shouldBe` Just (Note "some notes")

        it "can be within a period" $ do
            let t = Transaction (Account "MyBank") (mkDate 2020 4 5) (Just $ Note "some notes") (Just $ Name "a name") (Category "Groceries") (amount 48.07)
            t `withinPeriod` (Period (theDay 2020 1 1) (theDay 2020 4 30)) `shouldBe` True
            t `withinPeriod` (Period (theDay 2021 1 1) (theDay 2021 4 30)) `shouldBe` False

    describe "a list of Transactions" $ do
        it "can be totaled" $ do
            let ts = [ simplified 2020 4 5 "Training" 48.07 
                     , simplified 2020 4 7 "Food" 42.17 
                     , simplified 2020 4 9 "Business Expenses" 1000.00 ]
            totalTransactions ts `shouldBe` amount 1090.24

        describe "can have an average" $ do
            it "for a period of one months" $ do
                let ts = [ simplified 2020 4 5 "Training" 48.07 
                         , simplified 2020 4 7 "Training" 42.17 
                         , simplified 2020 4 9 "Training" 1000.00 ]
                summarizeTransactionsMonths 1 ts `shouldBe` 
                    (Category "Training",amount 1090.24, amount 1090.24)
            it "for a two month period " $ do
                let ts = [ simplified 2020 4 1 "Training" 48.07 
                         , simplified 2020 4 7 "Training" 42.17 
                         , simplified 2020 5 1 "Training" 1000.00 ]
                summarizeTransactionsMonths 2 ts `shouldBe` 
                    (Category "Training",amount 1090.24, amount 545.12)

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

    
    describe "transactionIntersect" $ do
        it "tells the transactions that are common date, name and amount in two transaction lists" $ do
            let t1 = Transaction { transactionAccount = Account "MyBank"
                                 , transactionDate    = theDay 2020 6 1
                                 , transactionNotes   = Just $ Note "some notes"
                                 , transactionName    = Just $ Name "Joe's shop"
                                 , transactionCategory = Category "Groceries"
                                 , transactionAmount   = amount (-48.07)
                                 }
            let t2 = Transaction { transactionAccount = Account "Investment"
                                 , transactionDate    = theDay 2020 5 1
                                 , transactionNotes   = Just $ Note "a long category name indeed"
                                 , transactionName    = Just $ Name "Another very long name,Apple"
                                 , transactionCategory = Category "Devices"
                                 , transactionAmount   = amount (-1000.00)
                                 }
            let t3 = Transaction { transactionAccount = Account "posted"
                                 , transactionDate    = theDay 2020 7 1
                                 , transactionNotes   = Just $ Note "notes"
                                 , transactionName    = Just $ Name "Jack shop"
                                 , transactionCategory = Category "Groceries"
                                 , transactionAmount   = amount (-100.00)
                                 }
            let t4 = Transaction { transactionAccount = Account "posted"
                                 , transactionDate    = theDay 2020 8 1
                                 , transactionNotes   = Just $ Note "NOTES"
                                 , transactionName    = Just $ Name "General"
                                 , transactionCategory = Category "Devices"
                                 , transactionAmount   = amount (-50.00)
                                 }
            let t5 = Transaction { transactionAccount = Account "pending"
                                 , transactionDate    = theDay 2020 9 1
                                 , transactionNotes   = Just $ Note "bad transaction"
                                 , transactionName    = Just $ Name "General"
                                 , transactionCategory = Category "Devices"
                                 , transactionAmount   = amount (-40050.00)
                                 }
            let t6 = Transaction { transactionAccount = Account "forecasted"
                                 , transactionDate    = theDay 2020 9 1
                                 , transactionNotes   = Just $ Note "bad transaction"
                                 , transactionName    = Just $ Name "General"
                                 , transactionCategory = Category "Devices"
                                 , transactionAmount   = amount (-40050.00)
                                 }
            let t7 = Transaction { transactionAccount = Account "Already An Account"
                                 , transactionDate    = theDay 2020 8 1
                                 , transactionNotes   = Nothing
                                 , transactionName    = Just $ Name "General"
                                 , transactionCategory = Category "Devices"
                                 , transactionAmount   = amount (-50.00)
                                 }
            let import_list = [t1,t2,t3,t4] 
            let main_list   = [t3,t5,t6,t7]
            t4 == t7 `shouldBe` False -- t4 and t7 are equals in terms of Date, Name and Amount, while being different
            t4 `sameTransaction` t7 `shouldBe` True
            (import_list `transactionIntersect` main_list) `shouldBe` [t3,t4] 
            



