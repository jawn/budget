module ImportFileNameSpec
    where

import Test.Hspec
import ImportFileName
import System.Process
import System.Exit

spec :: SpecWith ()
spec = do
    describe "extract name" $ do
        it "can determine the account name from the file path" $ do
            extractName "/Users/foo/data/Savings202004.csv" 
                `shouldBe` Right "Savings"
            extractName "./Checking202004.csv" 
                `shouldBe` Right "Checking"

        it "cannot determine the account name if all digits" $ do
            extractName "/Users/foo/data/202004.csv" 
                `shouldBe` Left "the file /Users/foo/data/202004.csv doesn't contain an account name"


    describe "import directory" $ do
        it "finds all the file names to import from a directory" $ do
            r1 <- system "mkdir test/temp"
            r2 <- system "touch test/temp/Checking202004.csv"
            r3 <- system "touch test/temp/Savings202004.csv"
            result <- importDirectory "test/temp"
            r4 <- system "rm -r test/temp"
            [r1,r2,r3,r4] `shouldBe` [ExitSuccess,ExitSuccess,ExitSuccess,ExitSuccess]
            result `shouldBe` Right [ "test/temp/Checking202004.csv"
                                    , "test/temp/Savings202004.csv"
                                    ]

        it "yield a message if no files in the directory " $ do
            r1 <- system "mkdir test/temp"
            result <- importDirectory "test/temp"
            r2 <- system "rm -r test/temp"
            [r1,r2] `shouldBe` [ExitSuccess,ExitSuccess]
            result `shouldBe` Left "the directory test/temp doesn't contain import files"

