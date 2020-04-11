module Detail 
    where

import Transaction
import Category
import Account
import Amount
import Period
import ExitWithMsg
import Same

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

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-94s:%10s" ("TOTAL "++ (show p)) (show a) 

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

total 
    :: [Transaction]
    -> String
total ts = totalLabel (transactionsPeriod ts) (totalTransactions ts) 


detailTitle 
    :: Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> String
detailTitle Nothing Nothing Nothing = "Transactions (all)"
detailTitle fp c p = 
    "Transactions " 
    ++ (maybe "" ("from file: "++) fp)
    ++ (maybe "" (("with category: "++) . categoryName) c)
    ++ (maybe "" show p)

printDetail
    :: Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> Either String [Transaction]
    -> IO ()
printDetail transactionFilePath category period transactions = do
    either exitWithMsg processDetail transactions 
        where
            processDetail :: [Transaction] -> IO ()
            processDetail transactions = do 
                let selection = (maybe id (\c -> filter (\t -> same categoryName c (transactionCategory t))) category)
                              . (maybe id (\p -> filter (\t -> (transactionDate t) `within` p)) period)
                putStrLn (detailTitle transactionFilePath category period)
                putStr (unlines (detail (selection transactions)))
                putStrLn (total (selection transactions))
