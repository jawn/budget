{-# LANGUAGE OverloadedStrings #-}
module Account ( Account (..) )
    where

import Data.Csv           ( FromField
                          , ToField
                          , parseField
                          , toField
                          )
import Data.Text          ( strip
                          , unpack )
import Data.Text.Encoding ( decodeUtf8) 
import FieldToString

data Account = Account { accountName :: String }
    deriving (Eq,Ord,Show)

instance FromField Account where
    parseField "" = fail "account name required"
    parseField s = pure $ Account $ textToString s 
        where
        textToString = Data.Text.unpack . strip . decodeUtf8

instance ToField Account where
    toField = stringToField . accountName

