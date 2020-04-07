
module ExpenseSpec
    where
import Test.Hspec
import Amount
import Category
import Expense

spec = do
    describe "Expense" $ do
        it "has a date, category and amount" $ do
            let exp = Expense (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
            date exp `shouldBe` (mkDate 2020 4 5)
            category exp `shouldBe` (Category "Training")
            amount exp `shouldBe` (mkAmount 48.07)

    describe "a list of Expenses" $ do
        it "can be totaled" $ do
            let exps = [Expense (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
                       ,Expense (mkDate 2020 4 7) (Category "Food") (mkAmount 42.17)  
                       ,Expense (mkDate 2020 4 9) (Category "Business Expenses") (mkAmount 1000.00)]
            totalExpenses exps `shouldBe` mkAmount 1090.24

        it "can be summarized, as first (category, total amounts)" $ do
            let exps = [Expense (mkDate 2020 4 5) (Category "Training") (mkAmount 48.07)
                       ,Expense (mkDate 2020 4 7) (Category "Training") (mkAmount 42.17)  
                       ,Expense (mkDate 2020 4 9) (Category "Training") (mkAmount 1000.00)]
            summarizeExpenses exps `shouldBe` ((Category "Training"), mkAmount 1090.24)

        it "can determine a period" $ do
            let exps = [Expense (mkDate 2020 4 31) (Category "Training") (mkAmount 48.07)
                       ,Expense (mkDate 2020 4 7) (Category "Training") (mkAmount 42.17)  
                       ,Expense (mkDate 2020 3 4) (Category "Training") (mkAmount 1000.00)]
            expensesPeriod exps `shouldBe` (mkDate 2020 3 4, mkDate 2020 4 31)


                        
