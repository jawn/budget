{-# LANGUAGE OverloadedStrings #-}

module TransactionsCsvSpec
    where

import Test.Hspec

import Transaction
import Amount
import Category
import TransactionsCsv

import Data.Vector
import qualified Data.Vector as Vector (toList)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Time.Calendar

theDay = fromGregorian

spec = do
    describe "transactions" $ do
        it "can be decoded from a ByteString containing simple values" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,12\nposted,,02/24/2020,Adventures in Clean,TRANSFERWISE INC,Training,-1242.26\n"
                ts = decodeTransactions bs
            ts `shouldBe` Right 
                [ Transaction (theDay 2020 2 25) (Category {categoryName = "Online Services"}) (mkAmount 12)
                , Transaction (theDay 2020 2 24) (Category {categoryName = "Training"}) (mkAmount (-1242.26))
                ]
        it "can be decoded from a ByteString containing values with double minus sign" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,--12\n"
                ts = decodeTransactions bs
            ts `shouldBe` Right 
                [ Transaction (theDay 2020 2 25) (Category {categoryName = "Online Services"}) (mkAmount 12)
                ]
        it "can notify an error when decoded from ill-formed data" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,-1foo2\n"
                ts = decodeTransactions bs
            ts `shouldBe` Left 
                "parse error (Failed reading: conversion error: expected Double, got \"1foo2\" (incomplete field parse, leftover: [102,111,111,50])) at \"\\n\""

            let bs = "hello world\nthis is not a csv file\n"
                ts = decodeTransactions bs
            ts `shouldBe` Left 
                "parse error (Failed reading: conversion error: [\"hello world\"]) at \"\\nthis is not a csv file\\n\""

        it "can be imported from a csv file" $ do
            let bs = "posted,,02/25/2020,,GOOGLE  DOMAINS,Online Services,12\nposted,,02/24/2020,Adventures in Clean,TRANSFERWISE INC,Training,-1242.26\n"
                fp = "test/test-transactions.csv" 
            ByteString.writeFile fp bs
            ts <- decodeTransactionsFromFile fp
            ts `shouldBe` Right 
                [ Transaction (theDay 2020 2 25) (Category {categoryName = "Online Services"}) (mkAmount 12)
                , Transaction (theDay 2020 2 24) (Category {categoryName = "Training"}) (mkAmount (-1242.26))
                ]
        it "can notify an error when failing from importing from file" $ do
            let fp = "foo.csv"
            cats <- decodeTransactionsFromFile fp
            cats `shouldBe` Left 
                "foo.csv: openBinaryFile: does not exist (No such file or directory)"
