module CommandSpec
    where
import Test.Hspec
import Command
import Category
import Period
import Data.Time.Calendar

spec = do
    describe "Command" $ do
        describe "No command given" $ do
            it "means summary" $ do
                let args = words ""
                command args `shouldBe` Right (Summary Nothing Nothing) 
        describe "Summary" $ do
            it "recognize the summary command with a transaction file" $ do
                let args = words "summary -t foo.csv"
                command args `shouldBe` 
                    Right (Summary (Just "foo.csv") Nothing)

            it "recognize the summary command with any transaction file" $ do
                let args = words "summary -t bar.csv"
                command args `shouldBe` 
                    Right (Summary (Just "bar.csv") Nothing)

            it "recognize the summary command with a transaction file and a category file" $ do
                let args = words "summary -t bar.csv -c foo.csv"
                command args `shouldBe` 
                    Right (Summary (Just "bar.csv") (Just "foo.csv"))

            it "recognize the summary command with a only category file" $ do
                let args = words "summary -c bar.csv"
                command args `shouldBe` 
                    Right (Summary Nothing (Just "bar.csv"))

            it "recognize the summary command with no arguments" $ do
                let args = words "summary" 
                command args `shouldBe` 
                    Right (Summary Nothing Nothing)

        describe "Detail" $ do
            it "recognize the detail command with no arguments" $ do
                let args = words "detail"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing Nothing Nothing)
            it "recognize the detail command with a transaction file name argument" $ do
                let args = words "detail -t MyTransaction.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing Nothing)
            it "recognize the detail command with a category argument" $ do
                let args = words "detail -c Groceries"
                command args `shouldBe` 
                    Right (Detail Nothing (Just (Category "Groceries")) Nothing Nothing)
            it "recognize the detail command with a transaction file and a category arguments" $ do
                let args = words "detail -t MyTransactions.csv -c Groceries"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing Nothing)
            it "recognize the detail command with a category and a transaction arguments" $ do
                let args = words "detail -c Groceries -t MyTransactions.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing Nothing)
            it "recognize the detail command with a period argument" $ do
                let args = words "detail -p 04/30/2020 05/31/2020"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 30) (theDay 2020 05 31))) Nothing)
            it "recognize the detail command with a month argument" $ do
                let args = words "detail -m 2020 4"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (Period (theDay 2020 04 01) (theDay 2020 04 30))) Nothing)
            it "recognize the detail command with a sorting criteria argument" $ do
                let args = words "detail -s ADm"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing Nothing (Just "ADm"))

            it "doesn't recognize the detail command with a wrong sorting criteria argument" $ do
                let args = words "detail -s ADX"
                command args `shouldBe` 
                    (Left $ unlines [ "wrong sorting criteria: ADX"
                                    , "Available criteria are one or many of:"
                                    , "A : Account ascending (a : descending)"
                                    , "C : Category ascending (c : descending)"
                                    , "D : Date ascending (d : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    , "N : Name ascending (n : descending)"
                                    , "O : Notes ascending (o : descending)"
                                    ])

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
                let args = words "help"
                command args `shouldBe` Right Help

        it "recognize the command in uppercase or lowercase" $ do
            let args = words "SUmmary -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing)

        it "recognize a prefix of the command" $ do
            let args = words "SU -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing)
            let args = words "sum -t foo.csv" 
            command args `shouldBe` 
                Right (Summary (Just "foo.csv") Nothing)

        it "doesn't recognize a unknown command" $ do
            let args = words "foo bar.csv" 
            command args `shouldBe` 
                Left "unknown command: foo bar.csv"

