module CommandSpec
    where
import Test.Hspec
import Command

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
