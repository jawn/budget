module Import
    where

import Transaction
import Account
import Amount
import Name
import Data.Time
import ExitWithMsg
import Config
import TransactionsCsv

type Attributes = (Day, Maybe Name, Amount)


importTransactions :: String -> [Transaction] -> [Transaction] -> Either String [Transaction]

importTransactions name transactions importations 
    | importations `alreadyIn` transactions = Left "transactions already imported"
    | otherwise                             = Right (transactions ++ (changeAccount name . filterPosted) importations)


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
changeAccount name = 
    map ( \t -> t { transactionAccount = Account name } ) 


filterPosted
    :: [Transaction]
    -> [Transaction]
filterPosted =
    filter ((Account "posted" ==) . transactionAccount) 


