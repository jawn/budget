{-# LANGUAGE OverloadedStrings #-}
module Config
    where
import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)
import CatchShowIO

type Config = [(String, String)]


retrieve 
    :: String 
    -> Either String Config
retrieve = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> Either String (String,String)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,pair] -> Right (strip (Text.toUpper key),strip pair)
              _ ->          Left $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip

retrieveFromFile 
    :: FilePath
    -> IO (Either String Config)
retrieveFromFile filePath =
    catchShowIO (readFile filePath)
    >>= return . either Left retrieve
