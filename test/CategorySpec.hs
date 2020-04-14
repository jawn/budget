{-# LANGUAGE OverloadedStrings #-}
module CategorySpec
    where
import Test.Hspec
import Category
import Data.List

spec = do
    describe "Category" $ do
        it "has a name" $ do
            let c = Category "Training"
            categoryName c `shouldBe` "Training"

    describe "a list of categories" $ do
        it "can be sorted" $ do
            let cs = [ Category "Training"
                     , Category "Auto"
                     , Category "Business Expenses" ]
            sort cs `shouldBe` [ Category {categoryName = "Auto"}
                               , Category {categoryName = "Business Expenses"}
                               , Category {categoryName = "Training"} ]

