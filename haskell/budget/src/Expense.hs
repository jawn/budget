module Expense
    where

import Data.Dates
type Category = String
type Amount = Int
data Expense = Expense DateTime Category Amount 
    deriving (Eq, Show)

date y m d = DateTime y m d 0 0 0

category :: Expense -> Category
category (Expense _ c _ ) = c

amount :: Expense -> Amount
amount (Expense _ _ a) = a

