{-# LANGUAGE OverloadedStrings #-}
module Config
    where

import Message (Message)

import CatchShowIO
import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)
import Control.Monad.Except

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

fromStringE
    :: String 
    -> ExceptT Message IO Config
fromStringE = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> ExceptT Message IO (String,String)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,value] -> return (strip (Text.toUpper key),strip value)
              _ ->          throwError $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip
fromFile 
    :: FilePath
    -> IO (Either Message Config)
fromFile filePath =
    catchShowIO (readFile filePath)
    >>= return . either Left fromString

fromFileE 
    :: FilePath
    -> ExceptT Message IO Config
fromFileE = undefined

