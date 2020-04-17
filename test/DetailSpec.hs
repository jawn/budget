
module DetailSpec
    where
import Test.Hspec
import ShouldBeOutput (shouldBeLine, shouldBeOutput)
import Transaction
import Account
import Name
import Note
import Category
import Amount
import Period
import Detail
import Data.Time.Calendar
import Data.Ord
import Data.List

spec = do
    let t1 = Transaction { transactionAccount = Account "MyBank"
                         , transactionDate    = theDay 2020 6 1
                         , transactionNotes   = Just $ Note "some notes"
                         , transactionName    = Just (Name "Joe's shop")
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = amount (-48.07)
                         }
    let t2 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 5 1
                         , transactionNotes   = Just $ Note "a long category name indeed"
                         , transactionName    = Just (Name "Another very long name,Apple")
                         , transactionCategory = Category "Devices and other house utilities"
                         , transactionAmount   = amount (-1000.00)
                         }
    describe "detail" $ do
        it "show transactions" $ do
            take 2 (detail (sortBy (comparing transactionDate) [t1,t2])) `shouldBeOutput` 
                 ["Investment 05/01/2020 a long category name Another very long na Devices and other house utilities | -1000.00"

                 ,"MyBank 06/01/2020 some notes Joe's shop Groceries | -48.07"]

        it "show the total for the transactions, mentionning the min and max date " $ do
            lines (footer [t1,t2]) `shouldBeOutput` 
                 [ "TOTAL from 05/01/2020 to 06/01/2020 : -1048.07"
                 , "2 transactions"
                 ]

    describe "title" $ do
        it "show a header for the detail report for all transactions" $ do
            detailTitle Nothing Nothing Nothing `shouldBe` 
                "Transactions (all)"

        it "show a header for the detail report for a certain file" $ do
            detailTitle (Just "Foo.csv") Nothing Nothing `shouldBe` 
                "Transactions from file: Foo.csv"

        it "show a header for the detail report for a certain category" $ do
            detailTitle Nothing (Just (Category "Groceries")) Nothing `shouldBe` 
                "Transactions with category: Groceries"

        it "show a header for the detail report for given period" $ do
            detailTitle Nothing Nothing (Just (Period (theDay 2020 1 1) (theDay 2020 3 31))) `shouldBe` 
                "Transactions from 01/01/2020 to 03/31/2020"
