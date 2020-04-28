module TransactionSpec
    where
import Test.Hspec
import Amount
import Name
import Note
import Account
import Category
import Period
import Transaction

simplified :: Integer -> Int -> Int -> String -> Double -> Transaction
simplified y m d c a = Transaction { transactionAccount = Account "MyBank"
                                   , transactionDate = theDay y m d
                                   , transactionNotes = Just $ Note "some notes"
                                   , transactionName = Just $ Name "a name"
                                   , transactionCategory = Category c
                                   , transactionAmount = amount a }

spec :: SpecWith ()
spec = do
    describe "Transaction" $ do
        it "has a date, category and amount" $ do
            let t = simplified 2020 4 5 "Training" 48.07
            transactionDate     t `shouldBe` (theDay 2020 4 5)
            transactionCategory t `shouldBe` (Category "Training")
            transactionAmount   t `shouldBe` (amount 48.07)

        it "also has an account, notes, and a name" $ do
            let t = Transaction (Account "MyBank") (theDay 2020 4 5) (Just $ Note "some notes") (Just $ Name "a name") (Category "Groceries") (amount 48.07)
            transactionAccount  t `shouldBe` Account "MyBank"
            transactionName t  `shouldBe` Just (Name "a name")
            transactionNotes t `shouldBe` Just (Note "some notes")

        it "can be within a period" $ do
            let t = Transaction (Account "MyBank") (theDay 2020 4 5) (Just $ Note "some notes") (Just $ Name "a name") (Category "Groceries") (amount 48.07)
            t `withinPeriod` (Period (theDay 2020 1 1) (theDay 2020 4 30)) `shouldBe` True
            t `withinPeriod` (Period (theDay 2021 1 1) (theDay 2021 4 30)) `shouldBe` False


    

