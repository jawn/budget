{-# LANGUAGE OverloadedStrings #-}

module Configuration ( Configuration
                     , atKey
                     , fromFile
                     , fromString 
                     , valueAtKeyfromFile 
                     )
    where

import Domain
import Control.Monad.Except
import qualified Data.Text as Text (Text, toUpper, splitOn, strip, pack, unpack)

type Key = String
type Value = String
type Configuration = [(Key, Value)]

atKey :: Configuration -> Key -> Domain Value
cfg `atKey` key = case lookup key cfg of
                    Nothing -> throwError ( "key not found in configuration: " ++ key )
                    Just v -> return v

fromString :: String -> Domain Configuration
fromString = sequence . map toKeyValuePair . lines
    where
        toKeyValuePair :: String -> Domain (Key,Value)
        toKeyValuePair s = 
            case Text.splitOn ":" (Text.pack s) of
              [key,value] -> return (strip (Text.toUpper key),strip value)
              _ ->          throwError $  "error while reading config file: not a key/value pair: " ++ s
        strip :: Text.Text -> String
        strip = Text.unpack . Text.strip

fromFile :: FilePath -> Domain Configuration
fromFile filePath = do
    content <- (catchIODomain (readFile filePath) :: Domain String)
    config <- fromString content
    return config

valueAtKeyfromFile :: Key -> FilePath -> Domain Value      
valueAtKeyfromFile key filePath = do
    config <- fromFile filePath
    value <-  config `atKey` key
    return value


