{-# LANGUAGE OverloadedStrings #-}
module NoteSpec where

import Note

import Data.Csv
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "A note" $ do
        it "can be created from a non empty field" $ do
            let field = "foo"
            let note = runParser (parseField field)
            note `shouldBe` Right (Note "foo")

        it "can be created from an empty field" $ do
            let field = "  "
            let note = runParser (parseField field)
            note `shouldBe` Right (Note "")
