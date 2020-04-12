{-# LANGUAGE OverloadedStrings #-}
module TransactionsCsv
    where

import Transaction
import Category
import Amount
import Account
import Period
import Config
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

import Data.Text (Text, unpack)
import Data.Text.Encoding as Text
import Data.Maybe


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

instance FromRecord Transaction where
    parseRecord v
      | length v == 7 = Transaction <$> v .! 0 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .! 5 <*> v .! 6
      | otherwise = fail (show v)

instance FromField Account where
    parseField "" = fail "account name required"
    parseField s = (pure . Account . unpack . decodeLatin1) s

instance FromField Time.Day where
    parseField = parseTimeM True defaultTimeLocale "%m/%d/%Y" . unpack . decodeLatin1

instance FromField Category where
    parseField = pure . Category . unpack . decodeLatin1 

instance FromField Amount where
    parseField s | head (Char8.unpack s) == '-' = fmap negate $ parseField (Char8.pack (tail (Char8.unpack s)))
    parseField s = case runParser (parseField s :: Parser Double) of
                     Right n -> pure $ mkAmount n
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
    -> Either String [Transaction]
decodeTransactions = fmap toList . decode NoHeader

encodeTransactions
    :: [Transaction]
    ->  ByteString
encodeTransactions = encodeWith defaultEncodeOptions { encUseCrLf = False }

decodeTransactionsFromFile 
    :: FilePath 
    -> IO (Either String [Transaction])
decodeTransactionsFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeTransactions

encodeTransactionsToFile
    :: [Transaction]
    -> FilePath
    -> IO (Either String ())
encodeTransactionsToFile ts filePath =
    catchShowIO (ByteString.writeFile filePath (encodeTransactions ts))

encodeTransactionsToFileAdditive
    :: [Transaction]
    -> FilePath
    -> IO (Either String ())
encodeTransactionsToFileAdditive ts filePath =
    catchShowIO (ByteString.appendFile filePath (encodeTransactions ts))

retrieveTransactions 
    :: Config
    -> Maybe FilePath
    -> IO (Either String [Transaction])
retrieveTransactions cfg Nothing = do
    home <- getHomeDirectory
    let fp = maybeToEither ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf") (lookup "TRANSACTIONS" cfg) 
    either (return . Left) (retrieveTransactions cfg . Just) fp

retrieveTransactions cfg (Just fp) = do
    home <- getHomeDirectory
    (decodeTransactionsFromFile . canonical home) fp

saveTransactions 
    :: Config
    -> [Transaction]
    -> IO ()
saveTransactions cfg ts = do
    home <- getHomeDirectory
    let fp = maybeToEither ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf") (lookup "TRANSACTIONS" cfg) 
    case fp of
      Left msg -> exitWithMsg msg
      Right filePath -> do
          result <- encodeTransactionsToFileAdditive ts filePath
          case result of
            Left msg -> exitWithMsg msg
            Right _ -> return ()
        

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

canonical :: String -> FilePath -> FilePath
canonical home ('.':'~':fp) = home ++ fp
canonical _ fp = fp
