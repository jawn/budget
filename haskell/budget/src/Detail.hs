module Detail 
    where

import Transaction
import Category
import Account
import Amount
import Period
import ExitWithMsg

import Data.List 
import Data.Ord
import Text.Printf
import Data.Time
import qualified Data.Time as Time


detail 
    :: [Transaction] 
    -> [String]
detail = map prettyLine . sortBy (comparing transactionDate) 

prettyLine :: Transaction -> String
prettyLine t = printf "%-20s %10s %-20s %-20s %-20s|%10s"
             (take 20 (accountName (transactionAccount t)))
             (formatTime defaultTimeLocale "%m/%d/%Y" (transactionDate t))
             (take 20 (maybe "" id (transactionNotes t)))
             (take 20 (maybe "" id (transactionName t)))
             (take 20 (categoryName (transactionCategory t)))
             (show (transactionAmount t))

total 
    :: [Transaction]
    -> String
total ts = printf "%-94s:%10s" "TOTAL" (show (totalTransactions ts))
printDetail
    :: Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> Either String [Transaction]
    -> IO ()
printDetail fp c p ts = do
    either exitWithMsg processDetail ts
        where
            processDetail :: [Transaction] -> IO ()
            processDetail transactions = do
                putStr   (unlines (detail transactions))
                putStrLn (total transactions)

