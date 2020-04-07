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
import qualified Data.ByteString.Char8 as S8


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

instance FromField Time.Day where
    parseField = parseTimeM True defaultTimeLocale "%m/%d/%Y" . unpack . decodeLatin1

instance FromField Category where
    parseField s = pure $ Category $ unpack $ decodeLatin1 s

instance FromField Amount where
    parseField s = case runParser (parseField s :: Parser Double) of
                     Right n -> pure $ mkAmount n

                    
decodeExpenses :: ByteString -> Either String (Vector Expense)
decodeExpenses = decode NoHeader
