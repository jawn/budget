module Summary ( summary
               , summaryLines
               , summaryTitle
               )
    where

import Amount
import CategorySelection
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

summaryLines :: (Maybe Period) -> SortingCriteria -> CategorySelector -> [Transaction] -> [String]
summaryLines p criteria selector tr = categoryTotals (Period.months per) ts criteria ++ [totalLabel per (totalTransactions ts)]
    where
        ts = selection tr
        per = maybe (transactionsPeriod tr) id p
        selection = filter ((`within` per) . transactionDate) . filter selector

summaryTitle :: Maybe FilePath -> CategorySelection -> Period -> String
summaryTitle tra_fp cat_sel per = "Summary " ++ title tra_fp cat_sel per
    where
    title :: Maybe FilePath -> CategorySelection -> Period -> String
    title t cs p = intercalate " " 
        [ maybe "(main file)" ("for file: " ++) t
        , "for " ++ show cs
        , ""
        , show p
        ]

summary
    :: Maybe FilePath
    -> CategorySelection
    -> Maybe Period
    -> SortingCriteria
    -> CategorySelector
    -> [Transaction]
    -> [String]
summary tr_fp cat_sel per sct selector tr = 
    [ summaryTitle tr_fp cat_sel thePeriod ] 
    ++ summaryLines per sct selector tr
        where
            thePeriod = maybe (transactionsPeriod tr) id per

