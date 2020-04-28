module Summary ( summary
               , summaryLines
               , summaryTitle
               )
    where

import Amount
import Category
import Period
import Same
import Sorting
import SummaryLine
import Transaction
import TransactionList

import Data.List
import Data.Ord
import Text.Printf

type NbMonths = Integer
categoryTotals :: NbMonths -> [Transaction] -> SortingCriteria -> [String]
categoryTotals nbMonths ts criteria = 
        (map show
        . sortWith criteria
        . map (summarizeTransactionsMonths nbMonths)
        . groupBy (same transactionCategory) 
        . sortBy (comparing transactionCategory)) ts

sortWith :: SortingCriteria -> [SummaryLine] ->  [SummaryLine]
sortWith [] = id
sortWith (criterion:_) = sortBy $ summaryOrdering criterion

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (show p)) (show a) (show (a `divideBy` (Period.months p))) 

summaryLines :: (Maybe Period) -> SortingCriteria -> [Category] -> [Transaction] -> [String]
summaryLines p criteria categories tr = categoryTotals (Period.months per) ts criteria ++ [totalLabel per (totalTransactions ts)]
    where
        ts = selection tr
        per = maybe (transactionsPeriod tr) id p
        selection = filter ((`within` per) . transactionDate) . (filter (`withCategoryIn`  categories))

summaryTitle :: Maybe FilePath -> Maybe FilePath -> Maybe Category -> Period -> String
summaryTitle tra_fp cat_fp cat per = "Summary " ++ title tra_fp cat_fp cat per
    where
    title :: Maybe FilePath -> Maybe FilePath -> Maybe Category -> Period -> String
    title t c a p = intercalate " " 
        [ maybe "(main file)" ("for file: " ++) t
        , maybe "" ("for categories in the file: "++) c
        , maybe "" (("category: "++) . categoryName)  a
        , show p
        ]

summary
    :: Maybe FilePath
    -> Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> SortingCriteria
    -> [Category]
    -> [Transaction]
    -> [String]
summary tr_fp ca_fp cat per sct sel tr = 
    [ summaryTitle tr_fp ca_fp cat thePeriod ] 
    ++ summaryLines per sct selection tr
        where
            thePeriod = maybe (transactionsPeriod tr) id per
            selection = maybe sel pure cat

