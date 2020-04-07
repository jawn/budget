module Report
    where
import Data.Char
import Amount
import Data.List
import Expense
import Data.Dates
import Text.Printf
import Data.Ord

type FileName = String

same f a b = f a == f b

report :: [Expense] -> [String]
report = map (uncurry prettyLine) 
        . map summarizeExpenses
        . groupBy (same category) 
        . sortBy (comparing category) 

reportAllCategories :: [Expense] -> [String]
reportAllCategories exps = report exps ++ [prettyLine (totalLabel (expensesPeriod exps)) (totalExpenses exps)]

reportForPeriod :: Period -> [Expense] -> [String]
reportForPeriod p exps = report exps ++ [prettyLine (totalLabel p) (totalExpenses exps)]

totalLabel :: Period -> String
totalLabel p = printf "TOTAL %s" (prettyPeriod p) 

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = printf "from %s to %s" (formatDate d1) (formatDate d2)

formatDate :: DateTime -> String
formatDate (DateTime y m d _ _ _) = printf "%02d/%02d/%04d" m d y

reportForCategories :: (Category -> Bool) -> [Expense] -> [String]
reportForCategories isValid exps = reportForPeriod (expensesPeriod exps) selection
    where
        selection = filter (isValid . category) exps

prettyLine :: Category -> Amount -> String
prettyLine c a = printf "%-49s:%10s" c (show a)

reportTitle :: FileName -> Maybe FileName -> Period -> String
reportTitle name Nothing       p = printf "Report for file:%s (all categories) %s" name (prettyPeriod p)
reportTitle name1 (Just name2) p = printf "Report for file:%s (%s) %s" name1 name2 (prettyPeriod p)
