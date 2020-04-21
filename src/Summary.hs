module Summary ( summaryLines
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
-- swap summary and summaryLines, call summary from Budget
summary :: NbMonths -> [Transaction] -> SortingCriteria -> [String]
summary nbMonths ts criteria = 
        (map (\(c,a,m) -> summaryLine c a m) 
        . sortWith criteria
        . map (summarizeTransactionsMonths nbMonths)
        . groupBy (same transactionCategory) 
        . sortBy (comparing transactionCategory)) ts

sortWith :: SortingCriteria -> [SummaryLine] ->  [SummaryLine]
sortWith [] = id
sortWith [criterion] = sortBy $ summaryOrdering criterion

summaryForPeriod :: Period -> SortingCriteria -> [Transaction] -> [String]
summaryForPeriod p criteria ts = summary (Period.months p) ts criteria ++ [totalLabel p (totalTransactions ts)]

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (show p)) (show a) (show (a `divideBy` (Period.months p))) 

summaryLines :: (Maybe Period) -> SortingCriteria -> (Category -> Bool) -> [Transaction] -> [String]
summaryLines p criteria isValid ts = summaryForPeriod period criteria (selection ts)
    where
        period = maybe (transactionsPeriod ts) id p
        selection = filter ((`within` period) . transactionDate) . filter (isValid . transactionCategory) 

summaryLine :: Category -> Amount -> Amount -> String
summaryLine c a m = printf "%-49s:%10s |%10s" (categoryName c) (show a) (show m)

summaryTitle :: Maybe FilePath -> Maybe FilePath -> Period -> String
summaryTitle tra_fp cat_fp per = "Summary " ++ title tra_fp cat_fp per
    where
    title :: Maybe FilePath -> Maybe FilePath -> Period -> String
    title t c p = intercalate " " 
        [ maybe "(main file)" ("for file: " ++) t
        , maybe "(all categories)" ("for categories in the file: "++) c
        , show p
        ]

-- printSummary
--     :: Maybe FilePath
--     -> Maybe FilePath
--     -> Maybe Period
--     -> SortingCriteria
--     -> Either Message (Category -> Bool)
--     -> Either Message [Transaction]
--     -> IO ()
-- printSummary transactionFilePath categoryFilePath period criteria selector transactions = do
--     either exitWithMsg processSummary (transactions >>= checkNotEmpty)
--         where
--             processSummary :: [Transaction] -> IO ()
--             processSummary transactions = do
--                 let summary = case categoryFilePath of
--                             Nothing -> summaryLines (const True)
--                             Just _ -> case selector of
--                                         Right f -> summaryLines f 
--                                         Left msg -> error $ printf "while importing categories: %s" msg
--                 putStrLn (summaryTitle transactionFilePath categoryFilePath (maybe (transactionsPeriod transactions) id period))
--                 putStrLn (unlines (summary period criteria transactions))
