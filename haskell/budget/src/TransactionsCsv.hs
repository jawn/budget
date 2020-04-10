{-# LANGUAGE OverloadedStrings #-}
module TransactionsCsv
    where

import Transaction
import Category
import Amount
import Config

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
    , decode
    , FromField(parseField)
    , runParser
    , Parser
    )

import Control.Monad
import qualified Control.Monad as Monad (mzero)

instance FromRecord Transaction where
    parseRecord v
      | length v == 7 = Transaction <$> v .! 2
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

                    
decodeTransactions 
    :: ByteString 
    -> Either String (Vector Transaction)
decodeTransactions = decode NoHeader

decodeTransactionsFromFile 
    :: FilePath 
    -> IO (Either String (Vector Transaction))
decodeTransactionsFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeTransactions

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
    (fmap (fmap toList) . decodeTransactionsFromFile . canonical home) fp

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

canonical :: String -> FilePath -> FilePath
canonical home ('.':'~':fp) = home ++ fp
canonical _ fp = fp
