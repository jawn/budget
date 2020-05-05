module VersionNumberSpec 
    where

import VersionNumber

import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Version Number" $ do
        it "shows major, minor, and patch numbers" $ do
            let v = VersionNumber { major = 2
                                  , minor = 3
                                  , patch = 17 }
            show v `shouldBe` "v2.3.17"

