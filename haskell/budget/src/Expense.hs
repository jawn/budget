module Expense
    where

import Data.Dates
import Data.Time 
import qualified Data.Time as Time
import Amount
import Category
import Data.Ord
import Data.List

type Period = (Day, Day)

data Expense = Expense Day Category Amount 
    deriving (Eq, Ord, Show)

mkDate y m d = fromGregorian y m d 

category :: Expense -> Category
category (Expense _ c _ ) = c

amount :: Expense -> Amount
amount (Expense _ _ a) = a

date :: Expense -> Day
date (Expense d _ _) = d

totalExpenses :: [Expense] -> Amount
totalExpenses = totalAmount . map amount

summarizeExpenses :: [Expense] -> (Category, Amount)
summarizeExpenses exps = (category (head exps), totalExpenses exps)

expensesPeriod :: [Expense] -> Period
expensesPeriod exps = (date1, date2)
    where
        date1 = minimum dates
        date2 = maximum dates
        dates = sort $ map date exps
