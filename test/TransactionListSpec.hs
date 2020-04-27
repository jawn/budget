module TransactionListSpec 
    where

import Account
import Amount
import Category
import Date
import Name
import Note
import Transaction
import TransactionList

import Control.Monad.Except
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "TransationList" $ do
        it "can be read from file" $ do
            let fileContent = unlines [ "PersornalSavings,,03/13/2020,some note, INTEREST PAID~~00000~~~~00000~~0~~~~GT41,Interest,0.03" 
                                      , "JointSavings,,03/12/2020,, INTEREST PAID~~00000~~~~00000~~0~~~~GT41,Interest,0.01" ]
            writeFile "test/testdata.csv" fileContent
            transactions <- runExceptT (transactionsFromFile "test/testdata.csv")
            transactions `shouldBe` Right
                [ Transaction { transactionAccount = Account { accountName = "PersornalSavings"}
                              , transactionDate = (theDay 2020 03 13)
                              , transactionNotes = Just (Note "some note")
                              , transactionName = Just (Name "INTEREST PAID~~00000~~~~00000~~0~~~~GT41")
                              , transactionCategory = Category { categoryName = "Interest"}
                              , transactionAmount = amount 0.03
                              }

                , Transaction { transactionAccount = Account { accountName = "JointSavings"}
                              , transactionDate = (theDay 2020 03 12)
                              , transactionNotes = Nothing
                              , transactionName = Just (Name "INTEREST PAID~~00000~~~~00000~~0~~~~GT41")
                              , transactionCategory = Category { categoryName = "Interest"}
                              , transactionAmount = amount 0.01
                              }
                ]
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
