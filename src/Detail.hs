module Detail 
    where

import Account     ( Account (..) )
import Amount      ( Amount (..) )
import Category    ( Category (..) )
import ExitWithMsg ( exitWithMsg )
import Message     ( Message )
import Period
import Same
import Sorting
import Transaction

import Data.List   ( intercalate )
import Text.Printf ( printf )
import Data.Time   ( Day 
                   , defaultTimeLocale
                   , formatTime
                   )
import Data.Maybe  ( catMaybes )


detail 
    :: [Transaction] 
    -> [String]
detail = map prettyLine 

prettyLine :: Transaction -> String
prettyLine t = printf "%-20s %10s %-20s %-20s %-20s|%10s"
             (take 20 (accountName (transactionAccount t)))
             (formatTime defaultTimeLocale "%m/%d/%Y" (transactionDate t))
             (take 20 (maybe "" show (transactionNotes t)))
             (take 20 (maybe "" show (transactionName t)))
             (take 20 (categoryName (transactionCategory t)))
             (show (transactionAmount t))

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-94s:%10s" ("TOTAL "++ (show p)) (show a) 

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

lengthLabel :: Int -> String
lengthLabel n = printf "%d transactions" n

footer 
    :: [Transaction]
    -> String
footer ts = unlines [ totalLabel (transactionsPeriod ts) (totalTransactions ts) 
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
    -> SortingCriteria
    -> Either Message [Transaction]
    -> IO ()
printDetail filePath category period criteria transactions = do
    either exitWithMsg (processDetail criteria) (transactions >>= checkNotEmpty)
        where
            processDetail :: SortingCriteria -> [Transaction] -> IO ()
            processDetail criteria transactions = do 
                let selection = (maybe id (\c -> filter (\t -> same categoryName c (transactionCategory t))) category)
                              . (maybe id (\p -> filter (\t -> (transactionDate t) `within` p)) period)
                putStrLn (detailTitle filePath category period)
                either exitWithMsg (putStr . unlines . detail) (checkNotEmpty (sortWithCriteria criteria (selection transactions)))
                putStrLn $ (footer (selection transactions)) ++ maybe "main transaction file" id filePath
