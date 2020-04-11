
module DetailSpec
    where
import Test.Hspec
import Transaction
import Account
import Category
import Amount
import Detail
import Data.Time.Calendar

theDay = fromGregorian 
spec = do
    let t1 = Transaction { transactionAccount = Account "MyBank"
                         , transactionDate    = theDay 2020 6 1
                         , transactionNotes   = Just "some notes"
                         , transactionName    = Just "Joe's shop"
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = mkAmount (-48.07)
                         }
    let t2 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 5 1
                         , transactionNotes   = Just "a long category name indeed"
                         , transactionName    = Just "Another very long name,Apple"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = mkAmount (-1000.00)
                         }
    describe "detail" $ do
        it "show transactions" $ do
            take 2 (detail [t1,t2]) `shouldBe` 
                 ["Investment           05/01/2020 a long category name Another very long na Devices             |  -1000.00"

                 ,"MyBank               06/01/2020 some notes           Joe's shop           Groceries           |    -48.07"]

    describe "total" $ do
        it "show the total for the transactions" $ do
            total [t1,t2] `shouldBe` 
                 "TOTAL                                                                                         :  -1048.07"

