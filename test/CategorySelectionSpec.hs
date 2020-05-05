module CategorySelectionSpec
    where

import Account
import Amount
import Date
import Category
import CategorySelection
import Note
import Name
import SelectionType
import Transaction

import Test.Hspec
import Control.Monad.Except

spec :: SpecWith ()
spec = do
    describe "Category Selection" $ do
        describe "can be created" $ do
            it "by default for all categories" $ do
                let cs = categorySelection ""
                cs `shouldBe` AllCategories

            it "for one single category" $ do
                let cs = categorySelection "Transfers"
                cs `shouldBe` SingleCategory (Category "Transfers") Selected

            it "for a category selection file" $ do
                let cs = categorySelection "foo.csv" 
                cs `shouldBe` CategoriesFromFile "foo.csv" Selected

        describe "can be specified" $ do
            it "by default for selection" $ do
                let cs = categorySelection "Transfers"
                cs `shouldBe` SingleCategory (Category "Transfers") Selected

            it "for exclusion of a single category" $ do
                let cs = excluded $ categorySelection "Transfers"
                cs `shouldBe` SingleCategory (Category "Transfers") Excluded

            it "for exclusion of a list of categories" $ do
                let cs = excluded $ categorySelection "foo.csv"
                cs `shouldBe` CategoriesFromFile "foo.csv" Excluded

        describe "can be used to create a selection function" $ do
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
            let txs = [t1,t2]
            it "which selects everything for All Categories" $ do
                f <- runExceptT $ categorySelector $ categorySelection ""
                filter <$> f <*> Right txs `shouldBe` Right [t1,t2]

            it "which selects a single category" $ do
                f <- runExceptT $ categorySelector $ categorySelection "Groceries" 
                filter <$> f <*> Right txs `shouldBe` Right [t1]

            it "which excludes a single category" $ do
                f <- runExceptT $ categorySelector $ excluded $ categorySelection "Groceries" 
                filter <$> f <*> Right txs `shouldBe` Right [t2]

            it "which selects categories from a list read from a file" $ do
                writeFile "test/categories.csv" "Devices and other house utilities\nTransfers\n"
                f <- runExceptT $ categorySelector $ categorySelection "test/categories.csv"
                filter <$> f <*> Right txs `shouldBe` Right [t2]

            it "which excludes categories from a list read from a file" $ do
                writeFile "test/categories.csv" "Devices and other house utilities\nTransfers\n"
                f <- runExceptT $ categorySelector $ excluded $ categorySelection "test/categories.csv"
                filter <$> f <*> Right txs `shouldBe` Right [t1]

            it "which signals a failure if the file can't be opened" $ do
                f <- runExceptT $ categorySelector $ categorySelection "foo.csv"
                filter <$> f <*> Right txs `shouldBe`
                    Left "foo.csv: openBinaryFile: does not exist (No such file or directory)"

