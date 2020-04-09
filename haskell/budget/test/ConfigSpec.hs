module ConfigSpec
    where
import Test.Hspec
import Config

spec = do
    describe "Config" $ do
        it "can retrieve a list of keys and values from a content" $ do
            let content = unlines [ "KEY:Value"
                                  , "OTHER:Something Else" ]
            retrieve content `shouldBe` Right
                [ ("KEY", "Value")
                , ("OTHER", "Something Else")
                ]
        it "removes useless spaces from the key/value pair" $ do
            let content = unlines [ "  KEY  :  value  "
                                  , " OTHER  :  something else     " ]
            retrieve content `shouldBe` Right 
                [ ("KEY", "value")
                , ("OTHER", "something else")
                ]
        it "does not care about case for the key, but does care for the value" $ do
            let content = unlines [ "Key  :Value"
                                  , "oTher:Something Else" ]
            retrieve content `shouldBe` Right 
                [ ("KEY", "Value")
                , ("OTHER", "Something Else")
                ]
        it "signal an error in case of ill-formed pair" $ do
            let content = unlines [ "  key    value  "
                                  , " other  :  something else     " ]
            retrieve content `shouldBe` 
                Left "error while reading config file: not a key/value pair:   key    value  "

        it "can be retrieved from a text file" $ do
            let fileContent = "key:value\nother:other\n"
                fp = "test/test-config" 
            writeFile fp fileContent
            config <- retrieveFromFile fp
            config `shouldBe` Right
                [ ("KEY", "value")
                , ("OTHER", "other")
                ]
