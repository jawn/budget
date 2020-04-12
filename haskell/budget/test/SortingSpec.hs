module SortSpec
    where

import Test.Hspec
import Amount
import Category
import Account
import Transaction
import Sorting
import Period
import Data.List
import Data.Ord

ts = [ Transaction { transactionAccount  = Account "Savings"
                   , transactionDate     = theDay 2020 1 23
                   , transactionNotes    = Nothing
                   , transactionName     = Just "Chez François"
                   , transactionCategory = Category "Business Expenses"
                   , transactionAmount   = mkAmount 48.07
                   }
     , Transaction { transactionAccount  = Account "Expenses"
                   , transactionDate     = theDay 2020 4 23
                   , transactionNotes    = Nothing
                   , transactionName     = Just "Lego"
                   , transactionCategory = Category "Playing"
                   , transactionAmount   = mkAmount 23.17
                   }
     , Transaction { transactionAccount  = Account "Savings"
                   , transactionDate     = theDay 2020 3 23
                   , transactionNotes    = Nothing
                   , transactionName     = Just "Apple"
                   , transactionCategory = Category "Business Expenses"
                   , transactionAmount   = mkAmount 1024.00
                   }
     , Transaction { transactionAccount  = Account "Expenses"
                   , transactionDate     = theDay 2020 1 22
                   , transactionNotes    = Nothing
                   , transactionName     = Just "Disney"
                   , transactionCategory = Category "Entertainment"
                   , transactionAmount   = mkAmount 500.00
                   }
     ]

spec = do 
    describe "sorting detail creates sort criteria" $ do
        it "on amount" $ do
            let st = sortByCriteria "M" ts
                result = unwords $ map (show . transactionAmount) st
            result `shouldBe` "23.17 48.07 500.00 1024.00"

        it "on name then amount" $ do
            let st = sortByCriteria "NM" ts
                result = unwords $ map (\t -> (maybe "" id (transactionName t)) ++ " " ++ show (transactionAmount t)) st
            result `shouldBe` "Apple 1024.00 Chez Fran\231ois 48.07 Disney 500.00 Lego 23.17"

        it "on account then date" $ do
            let st = sortByCriteria "AD" ts
                result = unwords $ map (\t -> (accountName (transactionAccount t) ++ " " ++ (showDate (transactionDate t)))) st
            result `shouldBe` "Expenses 01/22/2020 Expenses 04/23/2020 Savings 01/23/2020 Savings 03/23/2020"

