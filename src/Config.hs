{-# LANGUAGE OverloadedStrings #-}
module Config
    where

import CatchShowIO
import Message (Message)

import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)

type Config = [(String, String)]


fromString 
    :: String 
    -> Either Message Config
fromString = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> Either Message (String,String)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,value] -> Right (strip (Text.toUpper key),strip value)
              _ ->          Left $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip

fromFile 
    :: FilePath
    -> IO (Either Message Config)
fromFile filePath =
    catchShowIO (readFile filePath)
    >>= return . either Left fromString
