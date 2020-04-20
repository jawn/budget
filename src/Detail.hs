module Detail 
    where

import Account     ( Account (..) )
import Amount      ( Amount (..) )
import Category    ( Category (..) )
import CategoriesCsv 
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
prettyLine t = printf "%-20s %10s %-20s %-20s %-40s|%10s"
             (take 20 (accountName (transactionAccount t)))
             (formatTime defaultTimeLocale "%m/%d/%Y" (transactionDate t))
             (take 20 (maybe "" show (transactionNotes t)))
             (take 20 (maybe "" show (transactionName t)))
             (take 40 (categoryName (transactionCategory t)))
             (show (transactionAmount t))

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-114s:%10s" ("TOTAL "++ (show p)) (show a) 

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
    -> Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> String
detailTitle Nothing Nothing Nothing Nothing = "Transactions (all)"
detailTitle fp cf c p = "Transactions " ++ intercalate " " (catMaybes options)
    where
        options = [ fmap ("from file: "++) fp
                  , fmap ("with categories from the file: "++) cf 
                  , fmap (("with category: "++) . categoryName) c
                  , fmap show p
                  ]

printDetail
    :: Maybe FilePath
    -> Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> SortingCriteria
    -> Either Message [Transaction]
    -> IO ()
printDetail filePath catFilePath category period criteria transactions = do
    either exitWithMsg (processDetail criteria) (transactions >>= checkNotEmpty)
        where
            processDetail :: SortingCriteria -> [Transaction] -> IO ()
            processDetail criteria transactions = do 
                categories <- case catFilePath of
                                Nothing -> return Nothing
                                Just fp -> do
                                    cats <- decodeCategoriesFromFile fp 
                                    case cats of
                                        Left msg -> error msg
                                        Right cats -> return $ Just cats 
                let selection = (maybe id (\cats -> filter (\t -> ((transactionCategory t) `elem` cats))) categories)
                              . (maybe id (\c -> filter (\t -> same categoryName c (transactionCategory t))) category)
                              . (maybe id (\p -> filter (\t -> (transactionDate t) `within` p)) period)
                putStrLn (detailTitle filePath Nothing category period)
                either exitWithMsg (putStr . unlines . detail) (checkNotEmpty (sortWithCriteria criteria (selection transactions)))
                putStrLn $ (footer (selection transactions)) ++ maybe "main transaction file" id filePath
