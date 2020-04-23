module Transaction ( Transaction (..)
                   , checkNotEmpty
                   , decodeTransactions
                   , decodeTransactionsFromFile
                   , encodeTransactions
                   , encodeTransactionsToFile
                   , fromPeriod
                   , retrieveTransactions
                   , retrieveTransactionsE
                   , retrieveTransactionsFromMainFile
                   , sameTransaction
                   , saveTransactions
                   , saveTransactionsE
                   , summarizeTransactionsMonths
                   , totalTransactions
                   , transactionIntersect
                   , transactionsPeriod
                   , withCategoryIn
                   , withinPeriod
    )
    where

import Control.Monad.Except
import Message ( Message )
import SummaryLine
import Amount
import Name
import Note
import Date
import Account
import Period
import Category
-- import DecodeString
import Data.List

import Config      ( Config )
import Data.Csv
    ( FromRecord(parseRecord)
    , (.!)
    , HasHeader(NoHeader)
    , EncodeOptions(..)
    , defaultEncodeOptions
    , decode
    , encodeWith
    , toRecord
    , record
    , ToRecord
    , toField
    )

import CatchShowIO
import System.Directory

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString


import qualified Data.Vector as Vector (toList)

data Transaction = Transaction { transactionAccount  :: Account
                               , transactionDate     :: Date
                               , transactionNotes    :: Maybe Note
                               , transactionName     :: Maybe Name
                               , transactionCategory :: Category
                               , transactionAmount   :: Amount
                               }
    deriving (Eq, Ord, Show)

totalTransactions :: [Transaction] -> Amount
totalTransactions = total . map transactionAmount

averageTransactionsPerMonth :: Integer -> [Transaction] -> Amount
averageTransactionsPerMonth ms ts = (totalTransactions ts) `divideBy` ms

summarizeTransactionsMonths :: Integer -> [Transaction] -> SummaryLine
summarizeTransactionsMonths ms ts =
    SummaryLine { summaryLineCategory = transactionCategory (head ts)
                , summaryLineAmount   = totalTransactions ts
                , summaryLineAverage  = averageTransactionsPerMonth ms ts
                }

transactionsPeriod :: [Transaction] -> Period
transactionsPeriod ts = Period date1 date2
    where
        date1 = minimum dates
        date2 = maximum dates
        dates = sort $ map transactionDate ts

checkNotEmpty :: [Transaction] -> Either Message [Transaction]
checkNotEmpty [] = Left "no transaction"
checkNotEmpty ts = Right ts

fromPeriod :: Period -> [Transaction] -> [Transaction]
fromPeriod p = filter ((`within` p) . transactionDate)

sameTransaction :: Transaction -> Transaction -> Bool
sameTransaction t1 t2 = transactionDate t1 == transactionDate t2
                    && transactionName t1 == transactionName t2
                    && transactionAmount t1 == transactionAmount t2

transactionIntersect :: [Transaction] -> [Transaction] -> [Transaction]
transactionIntersect  = intersectBy sameTransaction


withCategoryIn :: Transaction -> [Category] -> Bool
_ `withCategoryIn` [] = True
t `withCategoryIn` cats = transactionCategory t `elem` cats

withinPeriod :: Transaction -> Period -> Bool
withinPeriod t p = (transactionDate t) `within` p


instance FromRecord Transaction where
    parseRecord v
      | length v <  7  = fail (show v)
      | otherwise = Transaction
                        <$> v .!  0
                        <*> v .!  2
                        <*> v .!  3
                        <*> v .!  4
                        <*> v .!  5
                        <*> v .!  6

instance ToRecord Transaction
    where toRecord t = record [ toField (transactionAccount t)
                              , toField ByteString.empty -- dummy empty field
                              , toField (transactionDate t)
                              , toField (transactionNotes t)
                              , toField (transactionName t)
                              , toField (transactionCategory t)
                              , toField (transactionAmount t)
                              ]


decodeTransactions
    :: ByteString
    -> Either Message [Transaction]
decodeTransactions = fmap Vector.toList . decode NoHeader

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

decodeTransactionsFromFileE
    :: FilePath
    -> ExceptT Message IO [Transaction]
decodeTransactionsFromFileE filePath =
    ExceptT (catchShowIO (ByteString.readFile filePath))
    >>= ExceptT . return . decodeTransactions

encodeTransactionsToFile
    :: [Transaction]
    -> FilePath
    -> IO (Either Message ())
encodeTransactionsToFile ts filePath =
    catchShowIO (ByteString.writeFile filePath (encodeTransactions ts))

encodeTransactionsToFileE
    :: [Transaction]
    -> FilePath
    -> ExceptT Message IO ()
encodeTransactionsToFileE ts filePath =
    ExceptT $ catchShowIO (ByteString.writeFile filePath (encodeTransactions ts))

retrieveTransactions
    :: Config
    -> Maybe FilePath
    -> IO (Either Message [Transaction])
retrieveTransactions cfg fp = maybe (retrieveTransactionsFromMainFile cfg) decodeTransactionsFromFile fp

retrieveTransactionsE
    :: Config
    -> Maybe FilePath
    -> ExceptT Message IO [Transaction]
retrieveTransactionsE cfg Nothing = retrieveTransactionsFromMainFileE cfg
retrieveTransactionsE _   (Just fp) =  decodeTransactionsFromFileE fp

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

retrieveTransactionsFromMainFileE
    :: Config
    -> ExceptT Message IO [Transaction]
retrieveTransactionsFromMainFileE cfg = do
    home <- liftIO getHomeDirectory
    transactions <- maybe
        (fail ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf"))
        decodeTransactionsFromFileE
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

saveTransactionsE
    :: Config
    -> [Transaction]
    -> ExceptT Message IO ()
saveTransactionsE cfg transactions = do
    home <- liftIO getHomeDirectory
    maybe
      (fail ("error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf"))
      (encodeTransactionsToFileE transactions)
      (lookup "TRANSACTIONS" cfg)
