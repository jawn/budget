module CommandSpec
    where

import CategorySelection
import Command
import Sorting
import Category
import Period
import SelectionType

import Control.Monad.Except
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Command" $ do
        describe "No command given" $ do
            it "means summary" $ do
                let args = words ""
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right (Summary Nothing AllCategories  Nothing []) 

        describe "Summary" $ do
            it "recognize the summary command with a transaction file" $ do
                let args = words "summary -t foo.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right 
                    (Summary (Just "foo.csv") AllCategories  Nothing [])

            it "recognize the summary command with any transaction file" $ do
                let args = words "summary -t bar.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary (Just "bar.csv") AllCategories  Nothing [])

            it "recognize the summary command with a transaction file and a category file" $ do
                let args = words "summary -t bar.csv -c foo.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary (Just "bar.csv") (CategoriesFromFile "foo.csv" Selected)  Nothing [])

            it "recognize the summary command with a category argument" $ do
                let args = words "summary -c Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing (SingleCategory (Category "Groceries") Selected)  Nothing [])

            it "recognize the summary command with an excluded category argument" $ do
                let args = words "summary -x Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing (SingleCategory (Category "Groceries") Excluded)  Nothing [])

            it "recognize the summary command with a only category file" $ do
                let args = words "summary -c bar.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing (CategoriesFromFile "bar.csv" Selected)  Nothing [])

            it "recognize the summary command with a category named argument" $ do
                let args = words "summary category Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing (SingleCategory (Category "Groceries") Selected)  Nothing [])

            it "recognize the summary command with an excluded category named argument" $ do
                let args = words "summary except Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing (SingleCategory (Category "Groceries") Excluded)  Nothing [])



            it "recognize the summary command with no arguments" $ do
                let args = words "summary" 
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  Nothing [])

            it "recognize the summary command sorting options" $ do
                let args1 = words "summary -s M" 
                cmd1 <- runExceptT $ command args1
                cmd1 `shouldBe` 
                    Right (Summary Nothing AllCategories  Nothing [AmountAsc])

                let args2 = words "summary -s c" 
                cmd2 <- runExceptT $ command args2
                cmd2 `shouldBe` 
                    Right (Summary Nothing AllCategories  Nothing [CategoryDesc])
            it "doesn't recognize the summary command with a wrong sorting criteria argument" $ do
                let args = words "summary -s f"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    (Left $ unlines [ "not a sort criterion: f"
                                    , "Available criteria are one of:"
                                    , "C : Category ascending (c : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    ])
            it "recognize the summary command with named options" $ do
                let args = words "summary transactions bar.csv categories foo.csv sortby M"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary (Just "bar.csv") (CategoriesFromFile "foo.csv" Selected)  Nothing [AmountAsc])

            it "recognize the summary command with a period option" $ do
                let args = words "summary -p 01/01/2020 12/31/2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])

            it "recognize the summary command with a period named option" $ do
                let args = words "summary period 01/01/2020 12/31/2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])

            it "recognize the summary command with a month option" $ do
                let args = words "summary -m 2020 03"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 03 01) (theDay 2020 03 31))) [])

            it "recognize the summary command with a month named option" $ do
                let args = words "summary month 2020 03"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 03 01) (theDay 2020 03 31))) [])

            it "recognize the summary command with a year option" $ do
                let args = words "summary -y 2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])
            it "doesn't recognize the summary command with a wrong year option" $ do
                let args = words "summary -y foo"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Left "parser error: wrong year format: foo"
            it "recognize the summary command with a named year option" $ do
                let args = words "summary year 2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Summary Nothing AllCategories  (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])
        describe "Detail" $ do
            it "recognize the detail command with no arguments" $ do
                let args = words "detail"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing Nothing [])
            it "recognize the detail command with a transaction file name argument" $ do
                let args = words "detail -t MyTransaction.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing Nothing [])
            it "recognize the detail command with a transaction file named option" $ do
                let args = words "detail transactions MyTransaction.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing Nothing [])
            it "recognize the detail command with a category argument" $ do
                let args = words "detail -c Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a category named argument" $ do
                let args = words "detail category Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a transaction file and a category arguments" $ do
                let args = words "detail -t MyTransactions.csv -c Groceries"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a category and a transaction arguments" $ do
                let args = words "detail -c Groceries -t MyTransactions.csv"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a period argument" $ do
                let args = words "detail -p 04/30/2020 05/31/2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 04 30) (theDay 2020 05 31))) [])
            it "recognize the detail command with a named period argument" $ do
                let args = words "detail period 04/30/2020 05/31/2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 04 30) (theDay 2020 05 31))) [])
            it "recognize the detail command with a month argument" $ do
                let args = words "detail -m 2020 4"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 04 01) (theDay 2020 04 30))) [])
            it "recognize the detail command with a named month argument" $ do
                let args = words "detail month 2020 4"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 04 01) (theDay 2020 04 30))) [])
            it "recognize the detail command with a year argument" $ do
                let args = words "detail -y 2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])
            it "recognize the detail command with a named year argument" $ do
                let args = words "detail year 2020"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just (Period (theDay 2020 01 01) (theDay 2020 12 31))) [])
            it "recognize the detail command with a sorting criteria argument" $ do
                let args = words "detail -s ADm"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing Nothing [AccountAsc,DateAsc,AmountDesc])
            it "recognize the detail command with a named sorting criteria argument" $ do
                let args = words "detail sortby ADm"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    Right (Detail Nothing Nothing Nothing Nothing [AccountAsc,DateAsc,AmountDesc])

            it "doesn't recognize the detail command with a wrong sorting criteria argument" $ do
                let args = words "detail -s ADX"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    (Left  $ unlines [ "not a sort criterion: X"
                                     , "Available criteria are one or many of:"
                                     , "A : Account ascending (a : descending)"
                                     , "C : Category ascending (c : descending)"
                                     , "D : Date ascending (d : descending)"
                                     , "M : Amount ascending (m : descending)"
                                     , "N : Name ascending (n : descending)"
                                     , "O : Notes ascending (o : descending)"
                                     ])
            it "recognize the detail command with a categories file argument" $ do
                let args1 = words "detail -c IncomeCategories.csv"
                cmd1 <- runExceptT $ command args1
                cmd1 `shouldBe` 
                    Right (Detail Nothing (Just "IncomeCategories.csv") Nothing Nothing [])
                let args2 = words "detail -c BusinessExpenses.CSV"
                cmd2 <- runExceptT $ command args2
                cmd2 `shouldBe` 
                    Right (Detail Nothing (Just "BusinessExpenses.CSV") Nothing Nothing [])
            it "doesn't recognize a unknown option" $ do
                let args = words "detail -z"
                cmd <- runExceptT $ command args
                cmd `shouldBe` 
                    (Left $ "option unrecognized or incomplete: -z")

        describe "Import" $ do
            it "recognize the import command with two arguments" $ do
                let args = words "import myImport2020Feb.csv ChaseBank"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right (Import "myImport2020Feb.csv" (Just "ChaseBank"))

            it "recognize the import command with one argument" $ do
                let args = words "import Downloads"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right (Import "Downloads" Nothing)

            it "recognize the import command without at least one supplementary arguments" $ do
                let args = words "import"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Left "import: missing argument (import {<filename> <accountname> | <folder> }" 

        describe "Help" $ do
            it "recognize the help command" $ do
                let args = words "help foo bar"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right (Help ["foo","bar"])

        describe "Version" $ do 
            it "recognize the version command" $ do
                let args = words "version"
                cmd <- runExceptT $ command args
                cmd `shouldBe` Right Version

        it "recognize the command in uppercase or lowercase" $ do
            let args = words "SUmmary -t foo.csv" 
            cmd <- runExceptT $ command args
            cmd `shouldBe` 
                Right (Summary (Just "foo.csv") AllCategories Nothing [])

        it "recognize a prefix of the command" $ do
            let args1 = words "SU -t foo.csv" 
            cmd1 <- runExceptT $ command args1
            cmd1 `shouldBe` 
                Right (Summary (Just "foo.csv") AllCategories Nothing [])
            let args2 = words "sum -t foo.csv" 
            cmd2 <- runExceptT $ command args2
            cmd2 `shouldBe` 
                Right (Summary (Just "foo.csv") AllCategories Nothing [])

        it "doesn't recognize a unknown command" $ do
            let args = words "foo bar.csv" 
            cmd <- runExceptT $ command args
            cmd `shouldBe` 
                Left "unknown command: foo bar.csv"

