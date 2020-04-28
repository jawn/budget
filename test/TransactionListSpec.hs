module TransactionListSpec 
    where

import Account
import Amount
import Category
import Date
import Message
import Name
import Note
import Period
import SummaryLine
import Transaction
import TransactionList

import Control.Monad.Except
import Test.Hspec

simplified :: Integer -> Int -> Int -> String -> Double -> Transaction
simplified y m d c a = Transaction { transactionAccount = Account "MyBank"
                                   , transactionDate = theDay y m d
                                   , transactionNotes = Just $ Note "some notes"
                                   , transactionName = Just $ Name "a name"
                                   , transactionCategory = Category c
                                   , transactionAmount = amount a }

writeTestFile :: String -> IO (Either Message TransactionList)
writeTestFile content = do
    liftIO $ writeFile "test/transactions.csv" content
    runExceptT $ transactionsFromFile "test/transactions.csv"

spec :: SpecWith ()
spec = do
    describe "TransationList" $ do
        describe "can be read from file" $ do
            it "containing simple values" $ do
                let bs = "MyBank,,02/25/2020,some note,GOOGLE  DOMAINS,Online Services,12\nMyBank,,02/24/2020,,TRANSFERWISE INC,Training,-1242.26\n"
                ts <- writeTestFile bs
                ts `shouldBe` Right 
                    [ Transaction (Account "MyBank") (theDay 2020 02 25) (Just $ Note "some note") (Just $ Name "GOOGLE  DOMAINS") (Category "Online Services") (amount 12.00)
                    , Transaction (Account "MyBank") (theDay 2020 02 24) Nothing (Just $ Name "TRANSFERWISE INC") (Category "Training") (amount (-1242.26))
                    ]
            it "with useless space" $ do
                let bs = "  MyBank  ,  ,  02/25/2020  ,  some note  ,  GOOGLE  DOMAINS  ,  Online Services  ,  12\n"
                ts <- writeTestFile bs
                ts `shouldBe` Right 
                    [ Transaction (Account "MyBank") (theDay 2020 02 25) (Just $ Note "some note") (Just $ Name "GOOGLE  DOMAINS") (Category "Online Services") (amount 12.00)]
            it "with values with double minus sign" $ do
                let bs = "MyBank,,02/25/2020,,GOOGLE  DOMAINS,Online Services,--12\n"
                ts <- writeTestFile bs
                fmap (fmap transactionAmount) ts `shouldBe` Right [amount 12]

            it "with dates in an odd format" $ do
                let bs = "MyBank,,  2/ 5/20,,GOOGLE  DOMAINS,Online Services,-12\n"
                ts <- writeTestFile bs
                fmap (fmap transactionDate) ts `shouldBe` Right [theDay 2020 2 5]

            it "containing more than the required number of fields" $ do
                let bs = "MyBank,,02/05/2020,,GOOGLE  DOMAINS,Online Services,-12,\n"
                ts <- writeTestFile bs
                fmap length ts `shouldBe` Right 1

            it "and notify an error when decoded from ill-formed data" $ do
                let bs1 = "MyBank,,02/25/2020,,GOOGLE  DOMAINS,Online Services,-1foo2\n"
                ts1 <- writeTestFile bs1
                ts1 `shouldBe` Left 
                    "parse error (Failed reading: conversion error: expected Double, got \"1foo2\" (incomplete field parse, leftover: [102,111,111,50])) at \"\\n\""

                let bs2 = ",,02/25/2020,,GOOGLE  DOMAINS,Online Services,-12\n"
                ts2 <- writeTestFile bs2
                ts2 `shouldBe` Left
                    "parse error (Failed reading: conversion error: account name required) at \"\\n\""
                let bs3 = "hello world\nthis is not a csv file\n"
                ts3 <- writeTestFile bs3
                ts3 `shouldBe` Left 
                    "parse error (Failed reading: conversion error: [\"hello world\"]) at \"\\nthis is not a csv file\\n\""

        it "can be saved to a file" $ do
            let transactions = [ Transaction { transactionAccount = Account { accountName = "PersornalSavings"}
                                             , transactionDate = (theDay 2020 02 13)
                                             , transactionNotes = Just (Note "some note")
                                             , transactionName = Just (Name "INTEREST PAID~~00000~~~~00000~~0~~~~GT41")
                                             , transactionCategory = Category { categoryName = "Interest"}
                                             , transactionAmount = amount 0.03
                                             }

                               , Transaction { transactionAccount = Account { accountName = "JointSavings"}
                                             , transactionDate = (theDay 2020 04 12)
                                             , transactionNotes = Nothing
                                             , transactionName = Just (Name "INTEREST PAID~~00000~~~~00000~~0~~~~GT41")
                                             , transactionCategory = Category { categoryName = "Interest"}
                                             , transactionAmount = amount 0.01
                                             }
                               ]
            _ <- runExceptT (transactionsToFile "test/testdata.csv" transactions)
            fileContent <- readFile "test/testdata.csv" 
            fileContent `shouldBe`  unlines [ "PersornalSavings,,02/13/2020,some note,INTEREST PAID~~00000~~~~00000~~0~~~~GT41,Interest,0.03" 
                                            , "JointSavings,,04/12/2020,,INTEREST PAID~~00000~~~~00000~~0~~~~GT41,Interest,0.01" ]

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
                    SummaryLine (Category "Training") (amount 1090.24) (amount 1090.24)
            it "for a two month period " $ do
                let ts = [ simplified 2020 4 1 "Training" 48.07 
                         , simplified 2020 4 7 "Training" 42.17 
                         , simplified 2020 5 1 "Training" 1000.00 ]
                summarizeTransactionsMonths 2 ts `shouldBe` 
                    SummaryLine (Category "Training") (amount 1090.24) (amount 545.12)

        it "can determine a period" $ do
            let ts = [ simplified 2020 4 31 "Training" 48.07 
                     , simplified 2020 4 7  "Training" 42.17 
                     , simplified 2020 3 4  "Training" 1000.00 ]
            transactionsPeriod ts `shouldBe` Period (theDay 2020 3 4) (theDay 2020 4 31)

    describe "check not empty" $ do
        it "yields a message is transaction list is empty" $ do
            let ts = [simplified 2020 4 5 "Training" 48.07] 
            checkNotEmpty ts `shouldBe` Right ts
            checkNotEmpty [] `shouldBe` Left "no transaction" 
