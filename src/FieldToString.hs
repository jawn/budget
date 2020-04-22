module FieldToString ( fieldToString
                     , stringToField 
                    )
    where
import Data.Text          ( pack
                          , strip
                          , unpack )
import Data.Text.Encoding ( decodeUtf8
                          , encodeUtf8 ) 
import Data.Csv           ( Field )

fieldToString :: Field -> String
fieldToString = Data.Text.unpack . strip . decodeUtf8

stringToField :: String -> Field
stringToField = encodeUtf8 . Data.Text.pack 

