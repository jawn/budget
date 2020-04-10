module CommandSpec
    where
import Test.Hspec
import Command
import Category
import Period
import Data.Time.Calendar

theDay = fromGregorian
spec = do
    describe "Command" $ do
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
                    Right (Detail Nothing Nothing Nothing)
            it "recognize the detail command with a transaction file name argument" $ do
                let args = words "detail -t MyTransaction.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransaction.csv") Nothing Nothing)
            it "recognize the detail command with a category argument" $ do
                let args = words "detail -c Groceries"
                command args `shouldBe` 
                    Right (Detail Nothing (Just (Category "Groceries")) Nothing)
            it "recognize the detail command with a transaction file and a category arguments" $ do
                let args = words "detail -t MyTransactions.csv -c Groceries"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing)
            it "recognize the detail command with a category and a transaction arguments" $ do
                let args = words "detail -c Groceries -t MyTransactions.csv"
                command args `shouldBe` 
                    Right (Detail (Just "MyTransactions.csv") (Just (Category "Groceries")) Nothing)
            it "recognize the detail command with a full period argument" $ do
                let args = words "detail -p 04/30/2020 05/31/2020"
                command args `shouldBe` 
                    Right (Detail Nothing Nothing (Just (theDay 2020 04 30, theDay 2020 05 31)))

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

        it "doesn't recognize the absence of a command" $ do
            let args = []
            command args  `shouldBe` Left "no command given"
