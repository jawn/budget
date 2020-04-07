module Expense
    where

import Data.Dates
import Amount
import Data.Ord
import Data.List

type Category = String
type Period = (DateTime, DateTime)

data Expense = Expense DateTime Category Amount 
    deriving (Eq, Ord, Show)

mkDate y m d = DateTime y m d 0 0 0

category :: Expense -> Category
category (Expense _ c _ ) = c

amount :: Expense -> Amount
amount (Expense _ _ a) = a

date :: Expense -> DateTime
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
