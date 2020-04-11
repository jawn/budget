module ImportSpec
    where
import Test.Hspec
import Transaction
import Account
import Category
import Period
import Amount
import Import

spec = do
    let t1 = Transaction { transactionAccount = Account "MyBank"
                         , transactionDate    = theDay 2020 6 1
                         , transactionNotes   = Just "some notes"
                         , transactionName    = Just "Joe's shop"
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = mkAmount (-48.07)
                         }
    let t2 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 5 1
                         , transactionNotes   = Just "a long category name indeed"
                         , transactionName    = Just "Another very long name,Apple"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = mkAmount (-1000.00)
                         }
    let t3 = Transaction { transactionAccount = Account "posted"
                         , transactionDate    = theDay 2020 7 1
                         , transactionNotes   = Just "notes"
                         , transactionName    = Just "Jack shop"
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = mkAmount (-100.00)
                         }
    let t4 = Transaction { transactionAccount = Account "posted"
                         , transactionDate    = theDay 2020 8 1
                         , transactionNotes   = Just "NOTES"
                         , transactionName    = Just "General"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = mkAmount (-50.00)
                         }
    describe "import" $ do
        describe "append transactions" $ do
            it "from a new list to an existing list" $ do
                let existing = [t1,t2] 
                let to_import = [t3,t4]
                let result = importTransactions existing to_import "CreditFoo"
                fmap length result `shouldBe` Right 4
                fmap (map (number . transactionAmount) . (drop 2)) result
                    `shouldBe` Right [-10000,-5000]

            it "while setting the account name to the given name" $ do
                let existing = [t1,t2] 
                let to_import = [t3,t4]
                let result = importTransactions existing to_import "CreditFoo"
                fmap (map (accountName . transactionAccount) . (drop 2)) result
                    `shouldBe` Right ["CreditFoo","CreditFoo"]

            it "doesn't append transactions where status is pending" $ do
                let t5 = Transaction { transactionAccount = Account "pending"
                                     , transactionDate    = theDay 2020 9 1
                                     , transactionNotes   = Just "bad transaction"
                                     , transactionName    = Just "General"
                                     , transactionCategory = Category "Devices"
                                     , transactionAmount   = mkAmount (-40050.00)
                                     }
                let existing = [t1,t2] 
                let to_import = [t5,t3,t4]
                let result = importTransactions existing to_import "CreditFoo"
                fmap (filter (\t -> transactionAmount t == mkAmount (-40050.00))) result 
                    `shouldBe` Right []

            it "give a message is the import list has already been imported" $ do
                let existing = [t1,t2] 
                let to_import = [t1,t2]
                let result = importTransactions existing to_import "CreditFoo"
                result  `shouldBe` Left "transactions already imported"
