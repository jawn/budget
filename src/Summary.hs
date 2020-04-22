module Summary ( summary
               , summaryLines
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
categoryTotals :: NbMonths -> [Transaction] -> SortingCriteria -> [String]
categoryTotals nbMonths ts criteria = 
        (map (\(c,a,m) -> summaryLine c a m) 
        . sortWith criteria
        . map (summarizeTransactionsMonths nbMonths)
        . groupBy (same transactionCategory) 
        . sortBy (comparing transactionCategory)) ts

sortWith :: SortingCriteria -> [SummaryLine] ->  [SummaryLine]
sortWith [] = id
sortWith [criterion] = sortBy $ summaryOrdering criterion

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (show p)) (show a) (show (a `divideBy` (Period.months p))) 

summaryLines :: (Maybe Period) -> SortingCriteria -> [Category] -> [Transaction] -> [String]
summaryLines p criteria categories tr = categoryTotals (Period.months period) ts criteria ++ [totalLabel period (totalTransactions ts)]
    where
        ts = selection tr
        period = maybe (transactionsPeriod tr) id p
        selection = filter ((`within` period) . transactionDate) . (filter (`withCategoryIn`  categories))

summaryLine :: Category -> Amount -> Amount -> String
summaryLine c a m = printf "%-49s:%10s |%10s" (categoryName c) (show a) (show m)

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
    [ summaryTitle tr_fp ca_fp cat period ] 
    ++ summaryLines per sct selection tr
        where
            period = maybe (transactionsPeriod tr) id per
            selection = maybe sel pure cat

