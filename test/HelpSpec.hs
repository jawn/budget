module HelpSpec
    where

import Test.Hspec
import Help

spec = do
    describe "help topic" $ do
        it "determine the topic of help" $ do
            topic "summary" `shouldBe` TopicSummary 
            topic "DeT"     `shouldBe` TopicDetail
            topic "i"       `shouldBe` TopicImport
            topic "so"      `shouldBe` TopicSort
        it "gives the topic help if arg topic not found" $ do
            topic "foo" `shouldBe` TopicHelp 
