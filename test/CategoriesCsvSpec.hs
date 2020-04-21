{-# LANGUAGE OverloadedStrings #-}

module CategoriesCsvSpec
    where

import Test.Hspec

import Category
import CategoriesCsv

import qualified Data.ByteString.Lazy as ByteString

spec :: SpecWith ()
spec = do
    describe "categories" $ do
        it "can be decoded from a ByteString containing simple values" $ do
            let bs = "Traning\nBusiness Expenses\nAuto\n"
                cats = decodeCategories bs
            cats `shouldBe` Right [ Category {categoryName = "Traning"}
                                  , Category {categoryName = "Business Expenses"}
                                  , Category {categoryName = "Auto"}]

        it "can be decoded from a ByteString containing values in between quotes" $ do
            let bs = "\"Traning\"\n\"Business Expenses\"\n\"Auto\"\n"
                cats = decodeCategories bs
            cats `shouldBe` Right [ Category {categoryName = "Traning"}
                                  , Category {categoryName = "Business Expenses"}
                                  , Category {categoryName = "Auto"}]

        it "can notify an error when decoded from ill-formed data" $ do
            let bs = "Traning\nBusiness Expenses\"\nAuto\n"
                cats = decodeCategories bs
            cats `shouldBe` 
                Left "parse error (Failed reading: satisfy) at \"\\\"\\nAuto\\n\""

        it "can be imported from a csv file" $ do
            let bs = "Traning\nBusiness Expenses\nAuto\n"
                fp = "test/test-categories.csv" 
            ByteString.writeFile fp bs
            cats <- decodeCategoriesFromFile fp
            cats `shouldBe` Right [ Category {categoryName = "Traning"}
                                              , Category {categoryName = "Business Expenses"}
                                              , Category {categoryName = "Auto"}]

        it "can notify an error when failing from importing from file" $ do
            let fp = "foo.csv"
            cats <- decodeCategoriesFromFile fp
            cats `shouldBe` 
                Left "foo.csv: openBinaryFile: does not exist (No such file or directory)"

