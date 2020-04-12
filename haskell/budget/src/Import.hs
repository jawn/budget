module Import
    where

import Transaction
import Account
import Amount
import Data.Time
import ExitWithMsg
import Config
import TransactionsCsv

type Attributes = (Day, Maybe Name, Amount)

importTransactions :: [Transaction] -> [Transaction] -> String -> Either String [Transaction]
importTransactions ts imp name = 
   case alreadyIn (map attributes imp) (map attributes ts) of
     False -> Right $ ts ++ map (changeAccount (Account name)) 
        (filter (\t -> transactionAccount t == Account "posted") imp)
     True -> Left "transactions already imported"

    where
    changeAccount :: Account -> Transaction -> Transaction
    changeAccount a t = t { transactionAccount = a }
    
    alreadyIn :: [Attributes] -> [Attributes] -> Bool
    alreadyIn [] _ = False
    alreadyIn (t:ts) list = if t `elem` list then True else alreadyIn ts list

    attributes :: Transaction -> Attributes
    attributes t = (transactionDate t, transactionName t, transactionAmount t)

importTransactionFile :: Config -> [Transaction] -> [Transaction] -> String -> IO ()
importTransactionFile config ts imp name = do
    case importTransactions ts imp name of
      Right result -> saveTransactions config result
      Left msg -> exitWithMsg msg

