module Import ( importTransactions )
    where

import Account
import Message ( Message )
import Transaction

importTransactions 
    :: String 
    -> [Transaction] 
    -> [Transaction] 
    -> Either Message [Transaction]

importTransactions name transactions importations 
    | importations `transactionIntersect` transactions /= [] = Left "transactions already imported"
    | otherwise                             = Right (transactions ++ (changeAccount name . filterPostedOrAlreadyAccount) importations)

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

