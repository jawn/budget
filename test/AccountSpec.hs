module AccountSpec
    where

import Test.Hspec
import Account

spec = do
    describe "Account" $ do
        it "has a name" $ do
            let a = Account "MyBank"
            accountName a `shouldBe` "MyBank"

