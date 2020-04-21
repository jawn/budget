module AccountSpec
    where

import Account ( Account (..) )

import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Account" $ do
        it "has a name" $ do
            let a = Account "MyBank"
            accountName a `shouldBe` "MyBank"

