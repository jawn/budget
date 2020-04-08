{-# LANGUAGE OverloadedStrings #-}
module ExpensesCsv
    where

import Expense
import Category
import Amount

import Data.Time
import qualified Data.Time as Time

import CatchShowIO

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as Char8 (unpack, pack)


import Data.Vector (Vector)
import qualified Data.Vector as Vector 

import Data.Text (Text, unpack)
import Data.Text.Encoding as Text


import Data.Csv
    ( FromRecord(parseRecord)
    , (.!)
    , HasHeader(NoHeader)
    , decode
    , FromField(parseField)
    , runParser
    , Parser
    )

import Control.Monad
import qualified Control.Monad as Monad (mzero)

instance FromRecord Expense where
    parseRecord v
      | length v == 7 = Expense <$> v .! 2
                               <*> v .! 5
                               <*> v .! 6
      | otherwise = fail (show v)

instance FromField Time.Day where
    parseField = parseTimeM True defaultTimeLocale "%m/%d/%Y" . unpack . decodeLatin1

instance FromField Category where
    parseField s = pure $ Category $ unpack $ decodeLatin1 s

instance FromField Amount where
    parseField s | head (Char8.unpack s) == '-' = fmap negate $ parseField (Char8.pack (tail (Char8.unpack s)))
    parseField s = case runParser (parseField s :: Parser Double) of
                     Right n -> pure $ mkAmount n
                     Left msg -> fail msg

                    
decodeExpenses 
    :: ByteString 
    -> Either String (Vector Expense)
decodeExpenses = decode NoHeader

decodeExpensesFromFile 
    :: FilePath 
    -> IO (Either String (Vector Expense))
decodeExpensesFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeExpenses
