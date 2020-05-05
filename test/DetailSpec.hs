
module DetailSpec
    where
import Test.Hspec
import ShouldBeOutput 

import Account  ( Account (..) )
import Amount   ( amount )
import Category ( Category (..) )
import CategorySelection
import Detail   ( detail 
              , detailLines
              , detailTitle
              , footer
              )
import Name    ( Name (..) )
import Note    ( Note (..) )
import Period  ( Period (..)
               , theDay
               )
import Sorting ( SortCriterion (..) )
import Transaction ( Transaction (..) )
import SelectionType


spec :: SpecWith ()
spec = do
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
    describe "footer" $ do
        it "show the total for the transactions, mentioning the min and max date" $ do
            (footer Nothing [t1,t2]) `shouldBeOutput` 
                 [ "TOTAL from 05/01/2020 to 06/01/2020 : -1048.07"
                 , "2 transactions"
                 , "main transaction file"
                 ]
        it "show the total for the transactions, mentioning the transaction file used" $ do
            (footer (Just "foo.csv") [t1,t2]) `shouldBeOutput` 
                 [ "TOTAL from 05/01/2020 to 06/01/2020 : -1048.07"
                 , "2 transactions"
                 , "foo.csv"
                 ]
        it "show the total for an empty transaction list" $ do
            (footer (Just "foo.csv") []) `shouldBeOutput` 
                 [ "no transactions"
                 , "foo.csv"
                 ]
    describe "detail lines" $ do
        it "filter the transactions" $ do
            detailLines Nothing [] (const True) [t1,t2] `shouldBe` [t1,t2]

        it "for a given category" $ do
            let c = Category "Devices and other house utilities"
            detailLines Nothing [] ((c==).transactionCategory) [t1,t2] `shouldBe` [t2]

        it "for a given category selection" $ do
            let sel = [ Category "Groceries"
                      , Category "Credit Card Payments"
                      ]
            detailLines Nothing [] ((`elem` sel).transactionCategory) [t1,t2] `shouldBe` [t1]
                 
        it "for a given period" $ do
            let p = Period (theDay 2020 1 1) (theDay 2020 5 1)
            detailLines (Just p) [] (const True) [t1,t2] `shouldBe` [t2]

        it "in a an order given by sort criteria" $ do
            let sc = [AmountAsc]
            detailLines Nothing sc (const True) [t1,t2] `shouldBe` [t2,t1]

    describe "title" $ do
        it "show a header for the detail report for all transactions" $ do
            detailTitle Nothing AllCategories Nothing `shouldBe` 
                "Transactions (all)"

        it "show a header for the detail report for a certain file" $ do
            detailTitle (Just "Foo.csv") AllCategories Nothing `shouldBe` 
                "Transactions from file: Foo.csv for all categories"

        it "show a header for the detail report for a certain category" $ do
            detailTitle Nothing (SingleCategory (Category "Groceries") Selected) Nothing `shouldBe` 
                "Transactions for category: Groceries"

        it "show a header for the detail report for a certain category file" $ do
            detailTitle Nothing (CategoriesFromFile "IncomeCategories.csv" Selected) Nothing `shouldBe` 
                "Transactions for categories in the file: IncomeCategories.csv"

        it "show a header for the detail report for given period" $ do
            detailTitle Nothing AllCategories (Just (Period (theDay 2020 1 1) (theDay 2020 3 31))) `shouldBe` 
                "Transactions for all categories from 01/01/2020 to 03/31/2020"

    describe "detail" $ do
        it "show the whole report" $ do
            detail (Just "foo.csv") 
                   (CategoriesFromFile "bar.csv" Selected)
                   (Just (Period (theDay 2020 1 1) (theDay 2020 12 31))) 
                   []
                   ((Category "Groceries" ==) . transactionCategory)
                   [t1,t2] `shouldBeOutput` 
                           ["Transactions from file: foo.csv for categories in the file: bar.csv from 01/01/2020 to 12/31/2020"
                           , "MyBank 06/01/2020 some notes Joe's shop Groceries | -48.07"
                           , "TOTAL from 06/01/2020 to 06/01/2020 : -48.07"
                           , "1 transactions"
                           , "foo.csv"
                           ]



