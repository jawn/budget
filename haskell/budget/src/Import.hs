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


importTransactions :: [Transaction] -> [Transaction] -> String -> Either String [Transaction]

importTransactions transactions importations name 
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


importTransactionFile :: Config -> String -> [Transaction] -> [Transaction] -> IO (Either String Int)
importTransactionFile config name ts imp  = do
    case importTransactions ts imp name of
      Right result -> do 
         saveTransactions config result
         return $ Right (length imp)
         
      Left msg -> return $ Left msg

