module Report
    where
import Data.Char
import Amount
import Category
import Data.List
import Transaction
import Data.Dates
import Period
import Text.Printf
import Data.Ord
import Data.Time
import ExitWithMsg
import qualified Data.Time as Time

same f a b = f a == f b

report :: Integer -> [Transaction] -> [String]
report nbMonths ts = 
        (map (\(c,a,m) -> prettyLine c a m) 
        . map (summarizeTransactionsMonths nbMonths)
        . groupBy (same transactionCategory) 
        . sortBy (comparing transactionCategory)) ts

reportAllCategories :: [Transaction] -> [String]
reportAllCategories ts = report (Period.months (transactionsPeriod ts)) ts ++ [totalLabel (transactionsPeriod ts) (totalTransactions ts)]

reportForPeriod :: Period -> [Transaction] -> [String]
reportForPeriod p ts = report (Period.months p) ts ++ [totalLabel p (totalTransactions ts)]

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (prettyPeriod p)) (show a) (show (divideAmount a (Period.months p))) 

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = printf "from %s to %s" (formatDate d1) (formatDate d2)

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

reportForCategories :: (Category -> Bool) -> [Transaction] -> [String]
reportForCategories isValid ts = reportForPeriod period selection
    where
        period = transactionsPeriod ts 
        selection = filter (isValid . transactionCategory) ts

prettyLine :: Category -> Amount -> Amount -> String
prettyLine c a m = printf "%-49s:%10s |%10s" (categoryName c) (show a) (show m)

reportTitle :: Maybe FilePath -> Maybe FilePath -> Period -> String
reportTitle Nothing Nothing           p = printf "Report (all categories) %s" (prettyPeriod p)
reportTitle Nothing (Just name)       p = printf "Report (%s) %s" name (prettyPeriod p)
reportTitle (Just name) Nothing       p = printf "Report for file:%s (all categories) %s" name (prettyPeriod p)
reportTitle (Just name1) (Just name2) p = printf "Report for file:%s (%s) %s" name1 name2 (prettyPeriod p)

summary
    :: Maybe FilePath
    -> Maybe FilePath
    -> Either String (Category -> Bool)
    -> Either String [Transaction]
    -> IO ()
summary transactionFilePath categoryFilePath selector transactions = do
    either exitWithMsg processSummary transactions
        where
            processSummary :: [Transaction] -> IO ()
            processSummary transactions = do
                let report = case categoryFilePath of
                            Nothing -> reportAllCategories
                            Just _ -> case selector of
                                        Right f -> reportForCategories f
                                        Left msg -> error $ printf "while importing categories: %s" msg
                putStrLn (reportTitle transactionFilePath categoryFilePath (transactionsPeriod transactions))
                putStrLn (unlines (report transactions))
