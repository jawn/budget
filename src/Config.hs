{-# LANGUAGE OverloadedStrings #-}
module Config
    where

import Message (Message)
import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)
import CatchShowIO

type Config = [(String, String)]


fromString 
    :: String 
    -> Either Message Config
fromString = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> Either Message (String,String)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,pair] -> Right (strip (Text.toUpper key),strip pair)
              _ ->          Left $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip

fromFile 
    :: FilePath
    -> IO (Either Message Config)
fromFile filePath =
    catchShowIO (readFile filePath)
    >>= return . either Left fromString
