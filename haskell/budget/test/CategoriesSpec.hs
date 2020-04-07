
module CategoriesSpec
    where
import Test.Hspec
import Categories

spec = do 
    describe "import Categories From List" $ do
        it "containing categories" $ do
            let contents = unlines ["Design Workshops"
                                   ,"Business Expenses"]
                cats = importCategoriesFromList contents
            cats `shouldBe` 
                Right ["Design Workshops"
                      ,"Business Expenses"]
        it "containing categories within double quotes" $ do
            let contents = unlines ["\"Design Workshops\""
                                   ,"\"Business Expenses\""]
                cats = importCategoriesFromList contents
            cats `shouldBe` 
                Right ["Design Workshops"
                      ,"Business Expenses"]

        it "and fail with a message if ill-formed list" $ do
            let contents = unlines ["\"Design Workshops\""
                                   ,"Business Expenses\""]
                cats = importCategoriesFromList contents
            cats `shouldBe` Left "error while reading categories: Business Expenses\""
