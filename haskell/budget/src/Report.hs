module Report
    where
import Data.Char
import Data.List
import Expense
import Data.Dates
import Text.Printf

type Period = (DateTime, DateTime)

report :: [Expense] -> [String]
report [] = []
report exps = map (uncurry pretty) totals ++ [pretty (totalLabel (period exps)) grandTotal]
    where 
        totals = map total groups
        total grp = (fst (head grp), sum (map snd grp))
        groups = groupBy (\a b -> fst a == fst b) categsAndAmounts
        categsAndAmounts = sort $ map (\exp -> (category exp, amount exp)) exps
        grandTotal = sum (map amount exps)

reportForPeriod :: Period -> [Expense] -> [String]
reportForPeriod _ [] = []
reportForPeriod p exps = map (uncurry pretty) totals ++ [pretty (totalLabel p) grandTotal]
    where 
        totals = map total groups
        total grp = (fst (head grp), sum (map snd grp))
        groups = groupBy (\a b -> fst a == fst b) categsAndAmounts
        categsAndAmounts = sort $ map (\exp -> (category exp, amount exp)) exps
        grandTotal = sum (map amount exps)

period :: [Expense] -> Period
period exps = (head dates, last dates) 
    where dates = sort (map date exps)
          date (Expense d _ _) = d


totalLabel :: Period -> String
totalLabel p = "TOTAL " ++ prettyPeriod p 

prettyPeriod :: Period -> String 
prettyPeriod (d1,d2) = "from " ++ formatDate d1 ++ " to " ++ formatDate d2

formatDate :: DateTime -> String
formatDate (DateTime y m d _ _ _) = printf "%02d/%02d/%04d" m d y

reportForCategories :: [Category] -> [Expense] -> [String]
reportForCategories cats exps = reportForPeriod (period exps) selection
    where 
    selection = filter (\exp -> category exp `elem` cats) exps

pretty :: Category -> Amount -> String
pretty c a = (prettyCategory c)  ++ ":" ++ (prettyAmount a)

prettyCategory :: Category -> String
prettyCategory c = take 49 (c ++ replicate 70 ' ')

prettyAmount :: Amount -> String
prettyAmount n = pad (sign n ++  reverse (prettyAmountPos 0 (abs n)))
    where 
        pad s = replicate p ' ' ++ s where p = 10 - length s
        sign n | n >= 0 = ""
               | otherwise = "-"
        prettyAmountPos 2 n = '.' : prettyAmountPos 3 n
        prettyAmountPos p 0 | p < 4 = '0' : prettyAmountPos (succ p) 0
        prettyAmountPos p 0 = ""
        prettyAmountPos p n = intToDigit (n `mod` 10) : prettyAmountPos (succ p) (n `div` 10)
       


