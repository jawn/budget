{-# LANGUAGE OverloadedStrings #-}

module ExpensesCsvSpec
    where

import Test.Hspec

import Expense
import Amount
import Category
import ExpensesCsv

import Data.Vector
import qualified Data.Vector as Vector (toList)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

spec = do
    describe "expenses" $ do
        it "can be decoded from a ByteString containing simple values" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,12\nposted,,02/24/2020,Adventures in Clean,TRANSFERWISE INC,Training,-1242.26\n"
                exps = fmap toList $ decodeExpenses bs
            exps `shouldBe` Right [ Expense (mkDate 2020 2 25) (Category {categoryName = "Online Services"}) (mkAmount 12)
                                  , Expense (mkDate 2020 2 24) (Category {categoryName = "Training"}) (mkAmount (-1242.26))
                                  ]

        it "can be decoded from a ByteString containing values with double minus sign" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,--12\nposted,,02/24/2020,Adventures in Clean,TRANSFERWISE INC,Training,-1242.26\n"
                exps = fmap toList $ decodeExpenses bs
            exps `shouldBe` Right [ Expense (mkDate 2020 2 25) (Category {categoryName = "Online Services"}) (mkAmount 12)
                                  , Expense (mkDate 2020 2 24) (Category {categoryName = "Training"}) (mkAmount (-1242.26))
                                  ]
