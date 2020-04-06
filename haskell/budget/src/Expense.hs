module Expense
    where

import Data.Dates
import Amount

type Category = String
data Expense = Expense DateTime Category Amount 
    deriving (Eq, Show)

mkDate y m d = DateTime y m d 0 0 0

category :: Expense -> Category
category (Expense _ c _ ) = c

amount :: Expense -> Amount
amount (Expense _ _ a) = a

date :: Expense -> DateTime
date (Expense d _ _) = d


