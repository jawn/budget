module Report
    where
import Data.Char
import Amount
import Data.List
import Expense
import Data.Dates
import Text.Printf

type Period = (DateTime, DateTime)
type FileName = String

report :: [Expense] -> [String]
report [] = []
report exps = map (uncurry prettyLine) totals 
    where 
        totals = map total groups
        total grp = (fst (head grp), totalAmount (map snd grp))
        groups = groupBy (\a b -> fst a == fst b) categsAndAmounts
        categsAndAmounts = sort $ map (\exp -> (category exp, amount exp)) exps

grandTotal :: [Expense] -> Amount
grandTotal = totalAmount . map amount

reportAllCategories :: [Expense] -> [String]
reportAllCategories exps = report exps ++ [prettyLine (totalLabel (period exps)) (grandTotal exps)]

reportForPeriod :: Period -> [Expense] -> [String]
reportForPeriod p exps = report exps ++ [prettyLine (totalLabel p) (grandTotal exps)]

period :: [Expense] -> Period
period exps = (head dates, last dates) 
    where dates = sort (map date exps)
          date (Expense d _ _) = d


totalLabel :: Period -> String
totalLabel p = printf "TOTAL %s" (prettyPeriod p) 

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = printf "from %s to %s" (formatDate d1) (formatDate d2)

formatDate :: DateTime -> String
formatDate (DateTime y m d _ _ _) = printf "%02d/%02d/%04d" m d y

reportForCategories :: (Category -> Bool) -> [Expense] -> [String]
reportForCategories isValid exps = reportForPeriod (period exps) selection
    where
        selection = filter (isValid . category) exps

prettyLine :: Category -> Amount -> String
prettyLine c a = printf "%-49s:%10s" c (show a)

prettyCategory :: Category -> String
prettyCategory c = take 49 (c ++ replicate 70 ' ')

reportTitle :: FileName -> Maybe FileName -> Period -> String
reportTitle name Nothing       p = printf "Report for file:%s (all categories) %s" name (prettyPeriod p)
reportTitle name1 (Just name2) p = printf "Report for file:%s (%s) %s" name1 name2 (prettyPeriod p)
