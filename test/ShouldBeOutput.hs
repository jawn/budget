module ShouldBeOutput ( shouldBeOutput, shouldBeLine )
    where

import Test.Hspec

shouldBeOutput :: [String] -> [String] -> Expectation
shouldBeOutput result expected = (map (unwords . words) result) `shouldBe`  expected

shouldBeLine :: String -> String -> Expectation
shouldBeLine result expected = ((unwords . words) result) `shouldBe`  expected

