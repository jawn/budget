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
            detailOrdering AmountAsc t1 t2 `shouldBe` GT
            detailOrdering AmountDesc t1 t2 `shouldBe` LT
            detailOrdering NameAsc t1 t2 `shouldBe` LT
            detailOrdering NameDesc t1 t2 `shouldBe` GT
            detailOrdering CategoryAsc t1 t3 `shouldBe` EQ
            detailOrdering CategoryDesc t1 t3 `shouldBe` EQ
            detailOrdering DateAsc t1 t3 `shouldBe` LT
            detailOrdering DateDesc t1 t3 `shouldBe` GT
            detailOrdering AccountAsc t2 t4 `shouldBe` EQ 
            detailOrdering AccountDesc t2 t4 `shouldBe` EQ 
            detailOrdering NotesAsc t1 t4 `shouldBe` GT
            detailOrdering NotesDesc t1 t4 `shouldBe` LT

    describe "orderings" $ do
        it "yields a list of orderings between two transactions, given a string" $ do
            detailOrderings t1 t2 [AccountAsc]  `shouldBe` GT
            detailOrderings t2 t4 [AccountAsc]  `shouldBe` EQ
            detailOrderings t2 t4 [AccountAsc, AmountAsc] `shouldBe` LT
            detailOrderings t2 t4 [AccountAsc, AmountDesc] `shouldBe` GT

    describe "sortWithCriteria" $ do
        it "sorts a list of transaction according to a set of criteria" $ do
            sortWithCriteria [AccountAsc,AmountDesc] ts `shouldBe` [t4,t2,t3,t1]

    describe "criteria" $ do
        it "reads a sort criteria for the detail command, given a string" $ do
            let result = [ AccountAsc
                         , AccountDesc
                         , CategoryAsc
                         , CategoryDesc
                         , DateAsc
                         , DateDesc
                         , AmountAsc
                         , AmountDesc
                         , NameAsc
                         , NameDesc
                         , NotesAsc
                         , NotesDesc
                         ]
            let input = "AaCcDdMmNnOo"
            readCriteria DetailSortingCriteria input `shouldBe` Right result 
        it "yields a message if a wrong criterion for the detail command is given" $ do
            let input = "ZAx"
            readCriteria DetailSortingCriteria input `shouldBe` Left "not a sort criterion: Z\nnot a sort criterion: x"
        it "reads a sort criteria for the summary command, given a string" $ do
            let result = [ CategoryAsc
                         , CategoryDesc
                         , AmountAsc
                         , AmountDesc
                         ]
            let input = "CcMm"
            map (readCriteria SummarySortingCriteria . pure) input `shouldBe` 
                [ Right [CategoryAsc]
                , Right [CategoryDesc]
                , Right [AmountAsc]
                , Right [AmountDesc]
                ]
        it "yields a message if a wrong criterion for the summary command is given" $ do
            let input = "N"
            readCriteria SummarySortingCriteria input `shouldBe` Left "not a sort criterion: N"

    describe "validateCriteria" $ do
        it "yields a message if not given a valid criteria for the detail command" $ do
            validateCriteria DetailSortingCriteria "ADX" `shouldBe` 
                    (Left $ unlines [ "not a sort criterion: X"
                                    , "Available criteria are one or many of:"
                                    , "A : Account ascending (a : descending)"
                                    , "C : Category ascending (c : descending)"
                                    , "D : Date ascending (d : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    , "N : Name ascending (n : descending)"
                                    , "O : Notes ascending (o : descending)"
                                    ])

        it "yields a message if not given a valid criteria for the summary command" $ do
            validateCriteria SummarySortingCriteria "M" `shouldBe` Right [AmountAsc]
            validateCriteria SummarySortingCriteria "m" `shouldBe` Right [AmountDesc]
            validateCriteria SummarySortingCriteria "C" `shouldBe` Right [CategoryAsc]
            validateCriteria SummarySortingCriteria "c" `shouldBe` Right [CategoryDesc]
            validateCriteria SummarySortingCriteria "MC" `shouldBe` 
                (Left $ unlines [ "too many criteria: MC" 
                                , "Available criteria are one of:"
                                , "C : Category ascending (c : descending)"
                                , "M : Amount ascending (m : descending)"
                                ])
            validateCriteria SummarySortingCriteria "z" `shouldBe` 
                    (Left $ unlines [ "not a sort criterion: z"
                                    , "Available criteria are one of:"
                                    , "C : Category ascending (c : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    ])
