module Summary ( printSummary
               , summaryAllCategories
               , summaryForCategories
               , summaryTitle
               )
    where

import Amount
import Category
import Transaction
import SummaryLine
import Sorting
import Message ( Message )
import Data.Char
import Data.List
import Data.Dates
import Period
import Text.Printf
import Data.Ord
import Data.Time
import ExitWithMsg
import qualified Data.Time as Time
import Same

type NbMonths = Integer

summary :: NbMonths -> [Transaction] -> SortingCriteria -> [String]
summary nbMonths ts criteria = 
        (map (\(c,a,m) -> prettyLine c a m) 
        . sortWith criteria
        . map (summarizeTransactionsMonths nbMonths)
        . groupBy (same transactionCategory) 
        . sortBy (comparing transactionCategory)) ts

sortWith :: SortingCriteria -> [SummaryLine] ->  [SummaryLine]
sortWith [] = id
sortWith [criterion] = sortBy $ summaryOrdering criterion

summaryAllCategories :: [Transaction] -> (Maybe Period) -> SortingCriteria -> [String]

summaryAllCategories ts Nothing criteria = summary (Period.months (transactionsPeriod ts)) ts criteria ++ [totalLabel (transactionsPeriod ts) (totalTransactions ts)]
summaryAllCategories ts (Just p) criteria = summary (Period.months p) selection criteria ++ [totalLabel p (totalTransactions selection)]
    where selection = fromPeriod p ts

summaryForPeriod :: Period -> [Transaction] -> SortingCriteria -> [String]
summaryForPeriod p ts criteria = summary (Period.months p) ts criteria ++ [totalLabel p (totalTransactions ts)]

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (show p)) (show a) (show (a `divideBy` (Period.months p))) 

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

summaryForCategories :: (Category -> Bool) -> [Transaction] -> (Maybe Period) -> SortingCriteria -> [String]
summaryForCategories isValid ts (Just p) criteria = summaryForPeriod p selection criteria
    where
        period = transactionsPeriod ts 
        selection = filter (isValid . transactionCategory) (fromPeriod p ts)
summaryForCategories isValid ts Nothing criteria = summaryForPeriod period selection criteria
    where
        period = transactionsPeriod ts 
        selection = filter (isValid . transactionCategory) ts

prettyLine :: Category -> Amount -> Amount -> String
prettyLine c a m = printf "%-49s:%10s |%10s" (categoryName c) (show a) (show m)

summaryTitle :: Maybe FilePath -> Maybe FilePath -> Period -> String
summaryTitle Nothing Nothing           p = printf "Report (all categories) %s" (show p)
summaryTitle Nothing (Just name)       p = printf "Report (%s) %s" name (show p)
summaryTitle (Just name) Nothing       p = printf "Report for file:%s (all categories) %s" name (show p)
summaryTitle (Just name1) (Just name2) p = printf "Report for file:%s (%s) %s" name1 name2 (show p)

printSummary
    :: Maybe FilePath
    -> Maybe FilePath
    -> Maybe Period
    -> SortingCriteria
    -> Either Message (Category -> Bool)
    -> Either Message [Transaction]
    -> IO ()
printSummary transactionFilePath categoryFilePath period criteria selector transactions = do
    either exitWithMsg processSummary (transactions >>= checkNotEmpty)
        where
            processSummary :: [Transaction] -> IO ()
            processSummary transactions = do
                let summary = case categoryFilePath of
                            Nothing -> summaryAllCategories
                            Just _ -> case selector of
                                        Right f -> summaryForCategories f 
                                        Left msg -> error $ printf "while importing categories: %s" msg
                putStrLn (summaryTitle transactionFilePath categoryFilePath (maybe (transactionsPeriod transactions) id period))
                putStrLn (unlines (summary transactions period criteria))
