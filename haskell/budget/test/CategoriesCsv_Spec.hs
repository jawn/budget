
module CategoriesCsv_Spec
    where
import Test.Hspec
import Categories

spec = do 
    describe "import Categories From List" $ do
        it "decode a csv string containing categories" $ do
            let csv = "\"Design Workshops\"\n\"Business Expenses\"\n"
                cats = importCategoriesFromList csv
            cats `shouldBe` 
                ["Design Workshops"
                ,"Business Expenses"]
