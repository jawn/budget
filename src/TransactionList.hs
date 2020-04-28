module TransactionList ( TransactionList
                       , averageTransactionsPerMonth
                       , checkNotEmpty
                       , fromPeriod
                       , totalTransactions
                       , summarizeTransactionsMonths
                       , transactionsFromFile
                       , transactionsPeriod
                       , transactionsToFile
                       , transactionIntersect
                       )
    where

import Amount
import Domain
import Message
import Period
import SummaryLine
import Transaction

import Data.Csv
    ( HasHeader(NoHeader)
    , decode
    , defaultEncodeOptions
    , encodeWith
    , encUseCrLf
    )
import Data.List



import qualified Data.Vector as Vector (toList)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString


type TransactionList = [Transaction]

transactionsFromFile :: FilePath -> Domain TransactionList
transactionsFromFile filePath = do
    content <- catchIODomain (ByteString.readFile filePath) 
    transactions <- transactionsFromByteString content
    return transactions

transactionsFromByteString :: ByteString -> Domain TransactionList
transactionsFromByteString = 
    domain . fmap Vector.toList . decode NoHeader

transactionsToFile :: FilePath -> TransactionList -> Domain ()
transactionsToFile filePath = 
   catchIODomain . (ByteString.writeFile filePath) . transactionsToByteString 

transactionsToByteString :: TransactionList -> ByteString
transactionsToByteString =
   encodeWith defaultEncodeOptions { encUseCrLf = False }

transactionIntersect :: TransactionList -> TransactionList -> TransactionList
transactionIntersect  = intersectBy sameTransaction


totalTransactions :: TransactionList -> Amount
totalTransactions = total . map transactionAmount

averageTransactionsPerMonth :: Integer -> TransactionList -> Amount
averageTransactionsPerMonth ms ts = (totalTransactions ts) `divideBy` ms

summarizeTransactionsMonths :: Integer -> TransactionList -> SummaryLine
summarizeTransactionsMonths ms ts =
    SummaryLine { summaryLineCategory = transactionCategory (head ts)
                , summaryLineAmount   = totalTransactions ts
                , summaryLineAverage  = averageTransactionsPerMonth ms ts
                }

transactionsPeriod :: TransactionList -> Period
transactionsPeriod ts = Period date1 date2
    where
        date1 = minimum dates
        date2 = maximum dates
        dates = sort $ map transactionDate ts

checkNotEmpty :: TransactionList -> Either Message TransactionList
checkNotEmpty [] = Left "no transaction"
checkNotEmpty ts = Right ts

fromPeriod :: Period -> TransactionList -> TransactionList
fromPeriod p = filter ((`within` p) . transactionDate)

