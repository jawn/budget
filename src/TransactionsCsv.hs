{-# LANGUAGE OverloadedStrings #-}
module TransactionsCsv ( decodeTransactions
                       , decodeTransactionsFromFile
                       , encodeTransactions
                       , encodeTransactionsToFile
                       , retrieveTransactions
                       , saveTransactions
    )
    where

import Message     ( Message )
import Transaction ( Transaction (..) )
import Category    ( Category (..) )
import Amount      ( Amount (..)
                   , amount )
import Account     ( Account (..) )
import Period      ( Period (..)
                   , showDate
                   , theDay )
import Config      ( Config (..) )
import Name        ( Name (..) )
import Note        ( Note (..) )
import ExitWithMsg

import Data.Time
import qualified Data.Time as Time

import CatchShowIO
import System.Directory

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Char8 as Char8 (unpack, pack)


import Data.Vector (Vector, toList)
import qualified Data.Vector as Vector 

import Data.Text (Text, unpack, strip)
import Data.Text.Encoding as Text
import Data.Maybe
import MaybeToEither
import Data.List.Split ( splitOn )
import Text.Printf

import Data.Csv
    ( FromRecord(parseRecord)
    , (.!)
    , HasHeader(NoHeader)
    , EncodeOptions(..)
    , defaultEncodeOptions
    , decode
    , encodeWith
    , FromField(parseField)
    , runParser
    , Parser
    , toRecord
    , record
    , ToRecord
    , ToField
    , toField
    )

import Control.Monad 
import qualified Control.Monad as Monad (mzero)

decodeString = unpack . strip . decodeLatin1 

instance FromRecord Transaction where
    parseRecord v
      | length v <  7  = fail (show v)
      | length v >= 7 = Transaction 
                        <$> v .!  account
                        <*> v .!  day
                        <*> v .!  notes
                        <*> v .!  name
                        <*> v .!  category 
                        <*> v .!  amount
                            where
                                [account, day, notes, name, category, amount] = [0,2,3,4,5,6]

instance FromField Name where
    parseField = pure . Name . decodeString

instance FromField Note where
    parseField = pure . Note . decodeString

instance FromField Account where
    parseField "" = fail "account name required"
    parseField s = (pure . Account . decodeString) s

instance FromField Time.Day where
    parseField = parseTimeM True defaultTimeLocale "%m/%d/%Y" . spaceToZero . decodeString
        where 
            spaceToZero s = case splitOn "/" s of
                              [m,d,y] -> printf "%02d/%02d/%04d" 
                                (read m :: Int) 
                                (read d :: Int ) 
                                (normalYear (read y))
                              _ -> s
normalYear :: Int -> Int
normalYear y 
    | y > 100 = y
    | y < 50  = 2000 + y
    | otherwise = 1900 + y

instance FromField Category where
    parseField = pure . Category . decodeString

instance FromField Amount where
    parseField s | head (Char8.unpack s) == '-' = fmap negate $ parseField (Char8.pack (tail (Char8.unpack s)))
    parseField s = case runParser (parseField s :: Parser Double) of
                     Right n -> pure $ amount n
                     Left msg -> fail msg

instance ToRecord Transaction
    where toRecord t = record [ toField (transactionAccount t)
                              , toField ByteString.empty -- dummy empty field
                              , toField (transactionDate t)
                              , toField (transactionNotes t)
                              , toField (transactionName t)
                              , toField (transactionCategory t)
                              , toField (transactionAmount t)
                              ]

instance ToField Name where
    toField (Name n) = Char8.pack $ n 

instance ToField Note where
    toField (Note n) = Char8.pack $ n

instance ToField Account where
    toField = Char8.pack . accountName

instance ToField Day where
    toField = Char8.pack . showDate

instance ToField Category where
    toField = Char8.pack . categoryName

instance ToField Amount where
    toField = Char8.pack . show

                    
decodeTransactions 
    :: ByteString 
    -> Either Message [Transaction]
decodeTransactions = fmap toList . decode NoHeader

encodeTransactions
    :: [Transaction]
    ->  ByteString
encodeTransactions = encodeWith defaultEncodeOptions { encUseCrLf = False }

decodeTransactionsFromFile 
    :: FilePath 
    -> IO (Either Message [Transaction])
decodeTransactionsFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeTransactions

encodeTransactionsToFile
    :: [Transaction]
    -> FilePath
    -> IO (Either Message ())
encodeTransactionsToFile ts filePath =
    catchShowIO (ByteString.writeFile filePath (encodeTransactions ts))

retrieveTransactions 
    :: Config
    -> Maybe FilePath
    -> IO (Either Message [Transaction])
retrieveTransactions cfg fp = maybe (retrieveTransactionsFromMainFile cfg) decodeTransactionsFromFile fp

retrieveTransactionsFromMainFile
    :: Config
    -> IO (Either Message [Transaction])
retrieveTransactionsFromMainFile cfg = do
    home <- getHomeDirectory
    transactions <- maybe 
        (fail ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf")) 
        decodeTransactionsFromFile 
        (lookup "TRANSACTIONS" cfg) 
    return transactions 

saveTransactions 
    :: Config
    -> [Transaction]
    -> IO (Either Message ())
saveTransactions cfg transactions = do
    home <- getHomeDirectory
    result <- maybe 
        (fail ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf")) 
        (encodeTransactionsToFile transactions) 
        (lookup "TRANSACTIONS" cfg) 
    return result
        

canonical :: String -> FilePath -> FilePath
canonical home ('.':'~':fp) = home ++ fp
canonical _ fp = fp
