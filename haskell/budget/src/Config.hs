{-# LANGUAGE OverloadedStrings #-}
module Config
    where
import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)
import CatchShowIO

type Config = [(String, String)]


fromString 
    :: String 
    -> Either String Config
fromString = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> Either String (String,String)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,pair] -> Right (strip (Text.toUpper key),strip pair)
              _ ->          Left $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip

fromFile 
    :: FilePath
    -> IO (Either String Config)
fromFile filePath =
    catchShowIO (readFile filePath)
    >>= return . either Left fromString
