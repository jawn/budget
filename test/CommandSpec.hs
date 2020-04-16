module CommandSpec
    where
import Test.Hspec
import Command
import Sorting
import Category
import Period
import Data.Time.Calendar

spec = do
    describe "Command" $ do
        describe "No command given" $ do
            it "means summary" $ do
                let args = words ""
                command args `shouldBe` Right (Summary Nothing Nothing []) 
        describe "Summary" $ do
            it "recognize the summary command with a transaction file" $ do
                let args = words "summary -t foo.csv"
                command args `shouldBe` 
                    Right (Summary (Just "foo.csv") Nothing [])

            it "recognize the summary command with any transaction file" $ do
                let args = words "summary -t bar.csv"
                command args `shouldBe` 
                    Right (Summary (Just "bar.csv") Nothing [])

            it "recognize the summary command with a transaction file and a category file" $ do
                let args = words "summary -t bar.csv -c foo.csv"
                command args `shouldBe` 
                    Right (Summary (Just "bar.csv") (Just "foo.csv") [])

            it "recognize the summary command with a only category file" $ do
                let args = words "summary -c bar.csv"
                command args `shouldBe` 
                    Right (Summary Nothing (Just "bar.csv") [])

            it "recognize the summary command with no arguments" $ do
                let args = words "summary" 
                command args `shouldBe` 
                    Right (Summary Nothing Nothing [])

            it "recognize the summary command sorting options" $ do
                let args = words "summary -s M" 
                command args `shouldBe` 
                    Right (Summary Nothing Nothing [AmountAsc])

                let args = words "summary -s c" 
                command args `shouldBe` 
                    Right (Summary Nothing Nothing [CategoryDesc])
            it "doesn't recognize the summary command with a wrong sorting criteria argument" $ do
                let args = words "summary -s f"
                command args `shouldBe` 
                    (Left $ unlines [ "not a sort criterion: f"
                                    , "Available criteria are one of:"
                                    , "C : Category ascending (c : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    ])
            it "recognize the summary command with named options" $ do
                let args = words "summary transactions bar.csv categories foo.csv sortby M"
                command args `shouldBe` 
                    Right (Summary (Just "bar.csv") (Just "foo.csv") [AmountAsc])

        describe "Detail" $ do
            it "recognize the detail command with no arguments" $ do
                let args = words "detail"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing Nothing [])
            it "recognize the detail command with a transaction file name argument" $ do
                let args = words "detail -t MyTransaction.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing [])
            it "recognize the detail command with a transaction file named option" $ do
                let args = words "detail transactions MyTransaction.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing [])
            it "recognize the detail command with a category argument" $ do
                let args = words "detail -c Groceries"
                command args `shouldBe` 
                    Right (Detail Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a category named argument" $ do
                let args = words "detail category Groceries"
                command args `shouldBe` 
                    Right (Detail Nothing (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a transaction file and a category arguments" $ do
                let args = words "detail -t MyTransactions.csv -c Groceries"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a category and a transaction arguments" $ do
                let args = words "detail -c Groceries -t MyTransactions.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing [])
            it "recognize the detail command with a period argument" $ do
                let args = words "detail -p 04/30/2020 05/31/2020"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 30) (theDay 2020 05 31))) [])
            it "recognize the detail command with a named period argument" $ do
                let args = words "detail period 04/30/2020 05/31/2020"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 30) (theDay 2020 05 31))) [])
            it "recognize the detail command with a month argument" $ do
                let args = words "detail -m 2020 4"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 01) (theDay 2020 04 30))) [])
            it "recognize the detail command with a named month argument" $ do
                let args = words "detail month 2020 4"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 01) (theDay 2020 04 30))) [])
            it "recognize the detail command with a sorting criteria argument" $ do
                let args = words "detail -s ADm"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing Nothing [AccountAsc,DateAsc,AmountDesc])
            it "recognize the detail command with a named sorting criteria argument" $ do
                let args = words "detail sortby ADm"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing Nothing [AccountAsc,DateAsc,AmountDesc])

            it "doesn't recognize the detail command with a wrong sorting criteria argument" $ do
                let args = words "detail -s ADX"
                command args `shouldBe` 
                    (Left  $ unlines [ "not a sort criterion: X"
                                     , "Available criteria are one or many of:"
                                     , "A : Account ascending (a : descending)"
                                     , "C : Category ascending (c : descending)"
                                     , "D : Date ascending (d : descending)"
                                     , "M : Amount ascending (m : descending)"
                                     , "N : Name ascending (n : descending)"
                                     , "O : Notes ascending (o : descending)"
                                     ])
            it "doesn't recognize a unknown option" $ do
                let args = words "detail -z"
                command args `shouldBe` 
                    (Left $ "option unrecognized or incomplete: -z")

        describe "Import" $ do
            it "recognize the import command with two arguments" $ do
                let args = words "import myImport2020Feb.csv ChaseBank"
                command args `shouldBe` Right (Import "myImport2020Feb.csv" (Just "ChaseBank"))

            it "recognize the import command with one argument" $ do
                let args = words "import Downloads"
                command args `shouldBe` Right (Import "Downloads" Nothing)

            it "recognize the import command without at least one supplementary arguments" $ do
                let args = words "import"
                command args `shouldBe` Left "import: missing argument (import {<filename> <accountname> | <folder> }" 

        describe "Help" $ do
            it "recognize the help command" $ do
                let args = words "help foo bar"
                command args `shouldBe` Right (Help ["foo","bar"])

        it "recognize the command in uppercase or lowercase" $ do
            let args = words "SUmmary -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing [])

        it "recognize a prefix of the command" $ do
            let args = words "SU -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing [])
            let args = words "sum -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing [])

        it "doesn't recognize a unknown command" $ do
            let args = words "foo bar.csv" 
            command args `shouldBe` 
                Left "unknown command: foo bar.csv"

