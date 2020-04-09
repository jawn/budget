module Report
    where
import Data.Char
import Amount
import Category
import Data.List
import Expense
import Data.Dates
import Period
import Text.Printf
import Data.Ord
import Data.Time
import ExitWithMsg
import qualified Data.Time as Time

same f a b = f a == f b

report :: Integer -> [Expense] -> [String]
report nbMonths exps = 
        (map (\(c,a,m) -> prettyLine c a m) 
        . map (summarizeExpensesMonths nbMonths)
        . groupBy (same expenseCategory) 
        . sortBy (comparing expenseCategory)) exps

reportAllCategories :: [Expense] -> [String]
reportAllCategories exps = report (Period.months (expensesPeriod exps)) exps ++ [totalLabel (expensesPeriod exps) (totalExpenses exps)]

reportForPeriod :: Period -> [Expense] -> [String]
reportForPeriod p exps = report (Period.months p) exps ++ [totalLabel p (totalExpenses exps)]

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s |%10s" ("TOTAL "++ (prettyPeriod p)) (show a) (show (divideAmount a (Period.months p))) 

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = printf "from %s to %s" (formatDate d1) (formatDate d2)

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

reportForCategories :: (Category -> Bool) -> [Expense] -> [String]
reportForCategories isValid exps = reportForPeriod period selection
    where
        period = expensesPeriod exps 
        selection = filter (isValid . expenseCategory) exps

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
    -> Either String [Expense]
    -> IO ()
summary expenseFilePath categoryFilePath selector expenses = do
    either exitWithMsg processSummary expenses
        where
            processSummary :: [Expense] -> IO ()
            processSummary expenses = do
                let report = case categoryFilePath of
                            Nothing -> reportAllCategories
                            Just _ -> case selector of
                                        Right f -> reportForCategories f
                                        Left msg -> error $ printf "while importing categories: %s" msg
                putStrLn (reportTitle expenseFilePath categoryFilePath (expensesPeriod expenses))
                putStrLn (unlines (report expenses))
