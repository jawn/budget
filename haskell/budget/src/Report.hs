module Report
    where
import Data.Char
import Amount
import Category
import Data.List
import Expense
import Data.Dates
import Text.Printf
import Data.Ord
import Data.Time
import qualified Data.Time as Time

same f a b = f a == f b

report :: [Expense] -> [String]
report = map (\(c,a,m) -> prettyLine c a m) 
        . map summarizeExpenses
        . groupBy (same category) 
        . sortBy (comparing category) 

reportAllCategories :: [Expense] -> [String]
reportAllCategories exps = report exps ++ [totalLabel (expensesPeriod exps) (totalExpenses exps)]

reportForPeriod :: Period -> [Expense] -> [String]
reportForPeriod p exps = report exps ++ [totalLabel p (totalExpenses exps)]

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-49s:%10s" ("TOTAL "++ (prettyPeriod p)) (show a)

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = printf "from %s to %s" (formatDate d1) (formatDate d2)

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

reportForCategories :: (Category -> Bool) -> [Expense] -> [String]
reportForCategories isValid exps = reportForPeriod (expensesPeriod exps) selection
    where
        selection = filter (isValid . category) exps

prettyLine :: Category -> Amount -> Amount -> String
prettyLine c a m = printf "%-49s:%10s |%10s" (categoryName c) (show a) (show m)

reportTitle :: FilePath -> Maybe FilePath -> Period -> String
reportTitle name Nothing       p = printf "Report for file:%s (all categories) %s" name (prettyPeriod p)
reportTitle name1 (Just name2) p = printf "Report for file:%s (%s) %s" name1 name2 (prettyPeriod p)
