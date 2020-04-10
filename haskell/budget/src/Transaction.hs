module Transaction
    where

import Data.Dates
import Data.Time.Calendar (Day,diffGregorianDurationClip, fromGregorian, CalendarDiffDays(..))
import Data.Time 
import qualified Data.Time as Time
import Amount
import Account
import Period
import Category
import Data.Ord
import Data.List


data Transaction = Transaction { transactionAccount  :: Account
                               , transactionDate     :: Day
                               , transactionNotes    :: Maybe String
                               , transactionName     :: Maybe String
                               , transactionCategory :: Category
                               , transactionAmount   :: Amount 
                               }
    deriving (Eq, Ord, Show)

totalTransactions :: [Transaction] -> Amount
totalTransactions = totalAmount . map transactionAmount

averageTransactionsPerMonth :: Integer -> [Transaction] -> Amount
averageTransactionsPerMonth months ts = divideAmount (totalTransactions ts) months

summarizeTransactionsMonths :: Integer -> [Transaction] -> (Category, Amount, Amount)
summarizeTransactionsMonths months ts = 
    (transactionCategory (head ts), totalTransactions ts, averageTransactionsPerMonth months ts) 

transactionsPeriod :: [Transaction] -> Period
transactionsPeriod ts = (date1, date2)
    where
        date1 = minimum dates
        date2 = maximum dates
        dates = sort $ map transactionDate ts
