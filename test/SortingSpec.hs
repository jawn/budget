{-# LANGUAGE OverloadedStrings #-}
module SortingSpec
    where

import Test.Hspec
import Amount
import Category
import Name
import Note
import Account
import Transaction
import Sorting
import Period
import Data.List
import Data.Ord

ts = [ Transaction { transactionAccount  = Account "Savings"
                   , transactionDate     = theDay 2020 1 23
                   , transactionNotes    = Just $ Note "some notes"
                   , transactionName     = Just $ Name "Chez François"
                   , transactionCategory = Category "Business Expenses"
                   , transactionAmount   = mkAmount 48.07
                   }
     , Transaction { transactionAccount  = Account "Expenses"
                   , transactionDate     = theDay 2020 4 23
                   , transactionNotes    = Nothing
                   , transactionName     = Just $ Name "Lego"
                   , transactionCategory = Category "Playing"
                   , transactionAmount   = mkAmount 23.17
                   }
     , Transaction { transactionAccount  = Account "Savings"
                   , transactionDate     = theDay 2020 3 23
                   , transactionNotes    = Nothing
                   , transactionName     = Just $ Name "Apple"
                   , transactionCategory = Category "Business Expenses"
                   , transactionAmount   = mkAmount 1024.00
                   }
     , Transaction { transactionAccount  = Account "Expenses"
                   , transactionDate     = theDay 2020 1 22
                   , transactionNotes    = Nothing
                   , transactionName     = Just $ Name "Disney"
                   , transactionCategory = Category "Entertainment"
                   , transactionAmount   = mkAmount 500.00
                   }
     ]

spec = do 
    let [t1,t2,t3,t4] = ts
    describe "foldOrdering" $ do
        it "folds a list of orderings, yielding an ordering" $ do
            foldOrdering [] `shouldBe` EQ
            foldOrdering [GT,LT] `shouldBe` GT
            foldOrdering [LT,GT] `shouldBe` LT
            foldOrdering [EQ,GT] `shouldBe` GT
            foldOrdering [EQ,EQ,LT] `shouldBe` LT
            foldOrdering [EQ,EQ,EQ] `shouldBe` EQ

    describe "ordering" $ do
        it "yields an ordering between two transactions, given a char" $ do
            ordering 'M' t1 t2 `shouldBe` GT
            ordering 'm' t1 t2 `shouldBe` LT
            ordering 'N' t1 t2 `shouldBe` LT
            ordering 'n' t1 t2 `shouldBe` GT
            ordering 'C' t1 t3 `shouldBe` EQ
            ordering 'c' t1 t3 `shouldBe` EQ
            ordering 'D' t1 t3 `shouldBe` LT
            ordering 'd' t1 t3 `shouldBe` GT
            ordering 'A' t2 t4 `shouldBe` EQ 
            ordering 'a' t2 t4 `shouldBe` EQ 
            ordering 'O' t1 t4 `shouldBe` GT
            ordering 'o' t1 t4 `shouldBe` LT

    describe "orderings" $ do
        it "yields a list of orderings between two transactions, given a string" $ do
            orderings t1 t2 "A"  `shouldBe` GT
            orderings t2 t4 "A"  `shouldBe` EQ
            orderings t2 t4 "AM" `shouldBe` LT
            orderings t2 t4 "Am" `shouldBe` GT

    describe "sortWithCriteria" $ do
        it "sorts a list of transaction according to a set of criteria" $ do
            sortWithCriteria "Am" ts `shouldBe` [t4,t2,t3,t1]

    describe "validateCriteria" $ do
        it "yields a message if not given a valid criteria for the detail command" $ do
            validateCriteria DetailSortingCriteria "MaD" `shouldBe` Right "MaD" 
            validateCriteria DetailSortingCriteria "ADX" `shouldBe` 
                    (Left $ unlines [ "wrong sorting criteria: ADX"
                                    , "Available criteria are one or many of:"
                                    , "A : Account ascending (a : descending)"
                                    , "C : Category ascending (c : descending)"
                                    , "D : Date ascending (d : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    , "N : Name ascending (n : descending)"
                                    , "O : Notes ascending (o : descending)"
                                    ])

        it "yields a message if not given a valid criteria for the summary command" $ do
            validateCriteria SummarySortingCriteria "Mc" `shouldBe` Right "Mc" 
            validateCriteria SummarySortingCriteria "ADX" `shouldBe` 
                    (Left $ unlines [ "wrong sorting criteria: ADX"
                                    , "Available criteria are one or two of:"
                                    , "C : Category ascending (c : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    ])
