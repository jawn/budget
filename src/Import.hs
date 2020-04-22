module Import ( importTransactions )
    where

import Message ( Message )
import Transaction
import Account
import Amount
import Name
import Date

type Attributes = (Date, Maybe Name, Amount)


importTransactions :: String -> [Transaction] -> [Transaction] -> Either Message [Transaction]

importTransactions name transactions importations 
    | importations `alreadyIn` transactions = Left "transactions already imported"
    | otherwise                             = Right (transactions ++ (changeAccount name . filterPostedOrAlreadyAccount) importations)


alreadyIn :: [Transaction] -> [Transaction] -> Bool

importations `alreadyIn` transactions =
    (map attributes importations) `attributesAlreadyIn` (map attributes transactions)


attributes :: Transaction ->  Attributes
attributes t = 
    (transactionDate t, transactionName t, transactionAmount t)

attributesAlreadyIn :: [Attributes] -> [Attributes] -> Bool

[] `attributesAlreadyIn` _ = False
(importation:importations) `attributesAlreadyIn` transactions 
  | importation `elem` transactions = True
  | otherwise                  = importations `attributesAlreadyIn` transactions 


changeAccount :: String -> [Transaction] ->  [Transaction]
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

