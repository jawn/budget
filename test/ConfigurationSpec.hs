module ConfigurationSpec where

import Configuration

import Control.Monad.Except
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "Configuration" $ do
        it "can be built from a string" $ do
            let content = unlines [ "KEY:Value"
                                  , "OTHER:Something Else"
                                  ]
            cfg <- runExceptT (fromString content)
            cfg `shouldBe` Right [ ("KEY","Value")
                                 , ("OTHER","Something Else")
                                 ]
        it "building does not care about case for the key, but does care for the value" $ do
            let content = unlines [ "Key  :Value"
                                  , "oTher:Something Else" ]
            cfg <- runExceptT (fromString content)
            cfg `shouldBe` Right [ ("KEY", "Value")
                                 , ("OTHER", "Something Else")
                                 ]

        it "signal an error in case of ill-formed pair" $ do
            let content = unlines [ "  key    value  "
                                  , " other  :  something else     " ]
            cfg <- runExceptT (fromString content)
            cfg `shouldBe`
                Left "error while reading config file: not a key/value pair:   key    value  "

        it "can be built from a text file" $ do
            let fileContent = "key:value\nother:other\n"
                fp = "test/test-config" 
            writeFile fp fileContent
            cfg <- runExceptT (fromFile fp)
            cfg `shouldBe` Right
                [ ("KEY", "value")
                , ("OTHER", "other")
                ]

        it "yields a message in case of failure while reading the text file" $ do
            cfg <- runExceptT (fromFile "foo")
            cfg `shouldBe` Left "foo: openFile: does not exist (No such file or directory)"

        it "can retrieve a value for a given key" $ do
            let cfg = [ ("KEY","Value")
                      , ("OTHER","Something Else")
                      ] 
            value <- runExceptT (cfg `atKey` "KEY")
            value `shouldBe` Right "Value"

        it "yields a message if the key can't be found" $ do
            let cfg = [ ("KEY","Value")
                      , ("OTHER","Something Else")
                      ] 
            value <- runExceptT (cfg `atKey` "FOO")
            value `shouldBe` Left "key not found in configuration: FOO"

        it "can retrieve a value for a given key from a given file" $ do
            let fileContent = "key:value\nother:other\n"
                fp = "test/test-config" 
            writeFile fp fileContent
            value <- runExceptT (valueAtKeyfromFile "KEY" fp)
            value `shouldBe` Right "value"
