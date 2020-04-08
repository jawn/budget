module Expense
    where

import Data.Dates
import Data.Time.Calendar (Day,diffGregorianDurationClip, fromGregorian, CalendarDiffDays(..))
import Data.Time 
import qualified Data.Time as Time
import Amount
import Period
import Category
import Data.Ord
import Data.List


data Expense = Expense { expenseDate     :: Day
                       , expenseCategory :: Category
                       , expenseAmount   :: Amount 
                       }
    deriving (Eq, Ord, Show)

totalExpenses :: [Expense] -> Amount
totalExpenses = totalAmount . map expenseAmount

averageExpensesPerMonth :: Integer -> [Expense] -> Amount
averageExpensesPerMonth months exps = divideAmount (totalExpenses exps) months

summarizeExpensesMonths :: Integer -> [Expense] -> (Category, Amount, Amount)
summarizeExpensesMonths months exps = 
    (expenseCategory (head exps), totalExpenses exps, averageExpensesPerMonth months exps) 

expensesPeriod :: [Expense] -> Period
expensesPeriod exps = (date1, date2)
    where
        date1 = minimum dates
        date2 = maximum dates
        dates = sort $ map expenseDate exps
