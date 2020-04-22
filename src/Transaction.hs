module Transaction ( Transaction (..)
                   , checkNotEmpty
                   , fromPeriod
                   , transactionIntersect
                   , sameTransaction
                   , summarizeTransactionsMonths
                   , totalTransactions
                   , transactionsPeriod
                   , withCategoryIn
                   , withinPeriod
    )
    where

import Message ( Message )
import Data.Dates
import Data.Time.Calendar (Day,diffGregorianDurationClip, fromGregorian, CalendarDiffDays(..))
import Data.Time 
import qualified Data.Time as Time
import Amount
import Name
import Note
import Account
import Period
import Category
import Data.Ord
import Data.List


data Transaction = Transaction { transactionAccount  :: Account
                               , transactionDate     :: Day
                               , transactionNotes    :: Maybe Note
                               , transactionName     :: Maybe Name
                               , transactionCategory :: Category
                               , transactionAmount   :: Amount 
                               }
    deriving (Eq, Ord, Show)

totalTransactions :: [Transaction] -> Amount
totalTransactions = total . map transactionAmount

averageTransactionsPerMonth :: Integer -> [Transaction] -> Amount
averageTransactionsPerMonth months ts = (totalTransactions ts) `divideBy` months

summarizeTransactionsMonths :: Integer -> [Transaction] -> (Category, Amount, Amount)
summarizeTransactionsMonths months ts = 
    (transactionCategory (head ts), totalTransactions ts, averageTransactionsPerMonth months ts) 

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
t `withCategoryIn` [] = True
t `withCategoryIn` cats = transactionCategory t `elem` cats

withinPeriod :: Transaction -> Period -> Bool
withinPeriod t p = (transactionDate t) `within` p
