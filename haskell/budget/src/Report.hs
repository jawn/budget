module Report
    where
import Data.Char
import Data.List
import Expense

report :: [Expense] -> [String]
report [] = []
report exps = map (uncurry pretty) totals ++ [pretty "TOTAL" grandTotal]
    where 
        totals = map total groups
        total grp = (fst (head grp), sum (map snd grp))
        groups = groupBy (\a b -> fst a == fst b) categsAndAmounts
        categsAndAmounts = sort $ map (\exp -> (category exp, amount exp)) exps
        grandTotal = sum (map amount exps)

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
       


