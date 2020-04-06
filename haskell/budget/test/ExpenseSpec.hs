
module ExpenseSpec
    where
import Test.Hspec
import Amount
import Expense

spec = do
    describe "Expense" $ do
        it "has a date, category and amount" $ do
            let exp = Expense (mkDate 2020 4 5) "Training" (mkAmount 48.07)
            date exp `shouldBe` (mkDate 2020 4 5)
            category exp `shouldBe` "Training"
            amount exp `shouldBe` (mkAmount 48.07)
