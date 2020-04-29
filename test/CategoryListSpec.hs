module CategoryListSpec
    where

import Category
import CategoryList
import Message

import Control.Monad.Except
import Test.Hspec

writeTestFile :: String -> IO (Either Message CategoryList)
writeTestFile content = do
    liftIO $ writeFile "test/categories.csv" content
    runExceptT $ categoriesFromFile "test/categories.csv"

spec :: SpecWith ()
spec = do
    describe "CategoryList" $ do
        describe "can be read from a file" $ do
            it "containing simple values" $ do
                let bs = "Traning\nBusiness Expenses\nAuto\n"
                cs <- writeTestFile bs
                cs `shouldBe` Right [ Category {categoryName = "Traning"}
                                    , Category {categoryName = "Business Expenses"}
                                    , Category {categoryName = "Auto"}]

            it "containing values in between quotes" $ do
                let bs = "\"Traning\"\n\"Business Expenses\"\n\"Auto\"\n"
                cs <- writeTestFile bs
                cs `shouldBe` Right [ Category {categoryName = "Traning"}
                                    , Category {categoryName = "Business Expenses"}
                                    , Category {categoryName = "Auto"}]

            it "notifying an error in case of ill-formed data" $ do
                let bs = "Traning\nBusiness Expenses\"\nAuto\n"
                cs <- writeTestFile bs
                cs `shouldBe` 
                    Left "parse error (Failed reading: satisfy) at \"\\\"\\nAuto\\n\""

            it "notifying an error when failing opening file" $ do
                let fp = "foo.csv"
                cs <- runExceptT $ categoriesFromFile fp
                cs `shouldBe` 
                    Left "foo.csv: openBinaryFile: does not exist (No such file or directory)"

