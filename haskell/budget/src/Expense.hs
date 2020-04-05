module Expense
    where

type Category = String
type Amount = Int
data Expense = Expense Category Amount 
    deriving (Eq, Show)

category :: Expense -> Category
category (Expense c _ ) = c

amount :: Expense -> Amount
amount (Expense _ a) = a

