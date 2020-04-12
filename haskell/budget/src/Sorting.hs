module Sorting 
    where

import Amount
import Category
import Account
import Transaction
import Period
import Data.Ord
import Data.List

sortByCriteria :: String -> [Transaction] -> [Transaction]
sortByCriteria "M" = sortBy (comparing transactionAmount)
sortByCriteria "NM" = sortBy (comparing (\t -> (transactionName t, transactionAmount t)))
sortByCriteria "AD" = sortBy (comparing (\t -> (transactionAccount t, transactionDate t)))
