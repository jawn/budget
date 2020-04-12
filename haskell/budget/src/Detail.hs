module Detail 
    where

import Transaction
import Category
import Account
import Amount
import Period
import ExitWithMsg
import Same
import Sorting

import Data.List 
import Data.Ord
import Text.Printf
import Data.Time
import qualified Data.Time as Time
import Data.Maybe


detail 
    :: [Transaction] 
    -> [String]
detail = map prettyLine 

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

lengthLabel :: Int -> String
lengthLabel n = printf "%d transactions" n

total 
    :: [Transaction]
    -> String
total ts = unlines [ totalLabel (transactionsPeriod ts) (totalTransactions ts) 
                   , lengthLabel (length ts)
                   ]


detailTitle 
    :: Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> String
detailTitle Nothing Nothing Nothing = "Transactions (all)"
detailTitle fp c p = "Transactions " ++ intercalate " " (catMaybes options)
    where
        options = [ fmap ("from file: "++) fp
                  , fmap (("with category: "++) . categoryName) c
                  , fmap show p
                  ]

printDetail
    :: Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> Either String [Transaction]
    -> Maybe SortingCriteria
    -> IO ()
printDetail transactionFilePath category period transactions sortingCriteria = do
    either exitWithMsg (processDetail (maybe "" id sortingCriteria)) (transactions >>= checkNotEmpty)
        where
            processDetail :: SortingCriteria -> [Transaction] -> IO ()
            processDetail sortingCriteria transactions = do 
                let selection = (maybe id (\c -> filter (\t -> same categoryName c (transactionCategory t))) category)
                              . (maybe id (\p -> filter (\t -> (transactionDate t) `within` p)) period)
                putStrLn (detailTitle transactionFilePath category period)
                either exitWithMsg (putStr . unlines . detail) (checkNotEmpty (sortWithCriteria sortingCriteria (selection transactions)))
                -- ((sortWithCriteria sortingCriteria (selection transactions)) >>= checkNotEmpty)
                putStrLn (total (selection transactions))