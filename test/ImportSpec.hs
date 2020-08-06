module ImportSpec
    where
import Test.Hspec
import ShouldBeOutput
import Transaction
import Name
import Note
import Account
import Category
import Period
import Amount
import Import

spec :: SpecWith ()
spec = do
    let t1 = Transaction { transactionAccount = Account "MyBank"
                         , transactionDate    = theDay 2020 6 1
                         , transactionNotes   = Just $ Note "some notes"
                         , transactionName    = Just $ Name "Joe's shop"
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = amount (-48.07)
                         }
    let t2 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 5 1
                         , transactionNotes   = Just $ Note "a long category name indeed"
                         , transactionName    = Just $ Name "Another very long name,Apple"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-1000.00)
                         }
    let t3 = Transaction { transactionAccount = Account "posted"
                         , transactionDate    = theDay 2020 7 1
                         , transactionNotes   = Just $ Note "notes"
                         , transactionName    = Just $ Name "Jack shop"
                         , transactionCategory = Category "Groceries"
                         , transactionAmount   = amount (-100.00)
                         }
    let t4 = Transaction { transactionAccount = Account "posted"
                         , transactionDate    = theDay 2020 8 1
                         , transactionNotes   = Just $ Note "NOTES"
                         , transactionName    = Just $ Name "General"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-50.00)
                         }
    let t5 = Transaction { transactionAccount = Account "pending"
                         , transactionDate    = theDay 2020 9 1
                         , transactionNotes   = Just $ Note "bad transaction"
                         , transactionName    = Just $ Name "General"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-40050.00)
                         }
    let t6 = Transaction { transactionAccount = Account "forecasted"
                         , transactionDate    = theDay 2020 9 1
                         , transactionNotes   = Just $ Note "bad transaction"
                         , transactionName    = Just $ Name "General"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-40050.00)
                         }
    let t7 = Transaction { transactionAccount = Account "Already An Account"
                         , transactionDate    = theDay 2020 9 1
                         , transactionNotes   = Nothing
                         , transactionName    = Just $ Name "General"
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount 48.07
                         }
    let t8 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 8 6
                         , transactionNotes   = Just $ Note "a long category name indeed"
                         , transactionName    = Just $ Name "A name that contains more than forty characters" 
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-1000.00)
                         }
    let t9 = Transaction { transactionAccount = Account "Investment"
                         , transactionDate    = theDay 2020 8 6
                         , transactionNotes   = Just $ Note "a long category name indeed"
                         , transactionName    = Just $ Name "A name that contains more than forty characters but is a different name" 
                         , transactionCategory = Category "Devices"
                         , transactionAmount   = amount (-1000.00)
                         }
    describe "import" $ do
        describe "append transactions" $ do
            it "from a new list to an existing list" $ do
                let existing = [t1,t2] 
                let to_import = [t3,t4]
                let result = importTransactions "CreditFoo" existing to_import 
                fmap (length . fst) result `shouldBe` Right 4
                fmap (map (number . transactionAmount) . (drop 2) . fst) result
                    `shouldBe` Right [-10000,-5000]

            it "while setting the account name to the given name" $ do
                let existing = [t1,t2] 
                let to_import = [t3,t4]
                let result = importTransactions "CreditFoo" existing to_import 
                fmap (map (accountName . transactionAccount) . (drop 2) .fst) result
                    `shouldBe` Right ["CreditFoo","CreditFoo"]

            it "doesn't append transactions where status is different from posted and not already an account" $ do
                let existing = [t1,t2] 
                let to_import = [t5,t3,t6,t4]
                let result = importTransactions "CreditFoo" existing to_import 
                fmap ((filter (\t -> transactionAmount t == amount (-40050.00))) . fst) result 
                    `shouldBe` Right []

            it "append transactions where status is already changed to Account (starting with an uppercase letter)" $ do
                let existing = [t1,t2] 
                let to_import = [t3,t7]
                let result = importTransactions "CreditFoo" existing to_import 
                fmap (map (accountName . transactionAccount) . fst) result
                    `shouldBe` Right ["MyBank", "Investment", "CreditFoo", "Already An Account"]

            it "should not import transactions already imported" $ do
                    let existing = [t1,t2] 
                    let to_import = [t1,t2]
                    let result = importTransactions "CreditFoo" existing to_import 
                    fmap (length . fst) result `shouldBe` Right 2 
                    fmap fst result `shouldBe` Right [ t1 , t2 ]
                    fmap snd result `shouldBe` Right [ t1 , t2 ]
            it "should not import transactions already imported based on same first 40 chars of the name" $ do
                    let existing = [t1,t8] 
                    let to_import = [t1,t9]
                    let result = importTransactions "CreditFoo" existing to_import 
                    fmap (length . fst) result `shouldBe` Right 2 
                    fmap fst result `shouldBe` Right [ t1 , t8 ]
                    fmap snd result `shouldBe` Right [ t1 , t9 ]
            it "should not import transactions that are duplicate in the import list" $ do
                    let existing = [t1,t2] 
                    let to_import = [t1,t8,t9]
                    let result = importTransactions "CreditFoo" existing to_import 
                    fmap (length . fst) result `shouldBe` Right 3
                    fmap fst result `shouldBe` Right [ t1 , t2, t8 ]
                    fmap snd result `shouldBe` Right [ t1 , t9 ]
            it "should not import transactions already imported even with status = posted" $ do
                    let existing = [t1,t2,t3] 
                    let to_import = [t1,t2,t3]
                    let result = importTransactions "CreditFoo" existing to_import 
                    fmap fst result `shouldBe` Right [ t1 , t2, t3 ]
                    fmap snd result `shouldBe` Right [ t1 , t2, t3 ]
            it "should import new transactions and ignore duplicates" $ do
                    let existing = [t1,t2] 
                    let to_import = [t1,t2,t3,t4]
                    let result = importTransactions "CreditFoo" existing to_import 
                    fmap (length . fst) result `shouldBe` Right 4 
                    fmap fst result `shouldBe` 
                        Right [ t1
                              , t2
                              , t3 { transactionAccount = Account "CreditFoo" }
                              , t4 { transactionAccount = Account "CreditFoo" }
                              ]
                    fmap snd result `shouldBe` Right [ t1, t2 ]
            it "should return duplicates too" $ do
                let existing = [t1,t2] 
                let to_import = [t1,t2,t3,t4]
                let result = importTransactions "CreditFoo" existing to_import 
                fmap (length . snd) result `shouldBe` Right 2
                fmap snd result `shouldBe` Right [ t1 , t2 ]

        describe "show duplicate" $ do
            it "show a list of transactions already present (Date, Name, Amount)" $ do
                let duplicates = [t1,t2,t3]
                showDuplicates duplicates `shouldBeOutput` 
                    [ "06/01/2020 Joe's shop -48.07"
                    , "05/01/2020 Another very long name,Apple -1000.00"
                    , "07/01/2020 Jack shop -100.00"
                    ]
