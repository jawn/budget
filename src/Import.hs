module Import ( importTransactions
              , showDuplicates )
    where

import Account
import Message ( Message )
import Transaction
import TransactionList

import Text.Printf ( printf )

importTransactions
    :: String 
    -> [Transaction] 
    -> [Transaction] 
    -> Either Message ([Transaction],[Transaction])
importTransactions name transactions importations = 
    Right ( transactions ++ 
            ( changeAccount name 
            $ filterPostedOrAlreadyAccount 
            $ filter (not . (`elem` duplicates)) importations)
          , duplicates )
    where
        duplicates :: [Transaction]
        duplicates = importations `transactionIntersect` transactions

showDuplicates :: [Transaction] -> [String]
showDuplicates = map showDuplicate
    where
    showDuplicate :: Transaction -> String
    showDuplicate t = printf "%10s %-60s %10s"
        (show (transactionDate t))
        (maybe "" show (transactionName t))
        (show (transactionAmount t))
changeAccount 
    :: String 
    -> [Transaction] 
    ->  [Transaction]
changeAccount name =  map change
    where
        change t 
          | ( not . alreadyAnAccountName . accountName . transactionAccount) t =
              t { transactionAccount = Account name } 
          | otherwise = t

filterPostedOrAlreadyAccount
    :: [Transaction]
    -> [Transaction]
filterPostedOrAlreadyAccount =
    filter ((\s -> (s == "posted" || alreadyAnAccountName s)) . accountName . transactionAccount)

alreadyAnAccountName :: String -> Bool
alreadyAnAccountName s = (s /= "posted") && (s /= "pending") && (s /= "forecasted") 

