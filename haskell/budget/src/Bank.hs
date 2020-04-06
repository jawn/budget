module Bank
    where
import Data.Csv 
import Data.List.Split
import Data.Dates
import qualified Data.Vector as V
import Expense
import Amount
import Report
import qualified Data.ByteString.Lazy as B (ByteString)

type CsvExpense = (String, String, String, String, String, String, String)

decodeExpenses :: B.ByteString -> Either String (V.Vector CsvExpense)
decodeExpenses = Data.Csv.decode NoHeader

importExpensesFromBank :: B.ByteString -> Either String [Expense]
importExpensesFromBank = fmap (map makeExpense . V.toList) . decodeExpenses
    where
        makeExpense (_,_,d,_,_,c,a) = Expense (stringToDate d) c (stringToAmount a)
        stringToAmount :: String -> Amount
        stringToAmount ('-':'-':s) = stringToAmount s -- because the bank sends double negative like "--42.00"
        stringToAmount s = case reads s :: [(Amount, String)] of
                                   [] -> error (s ++ " ???")
                                   [(a,_)] -> a
        stringToDate :: String -> DateTime
        stringToDate s = case splitOn "/" s of
                           [m,d,y] -> mkDate (read y) (read m) (read d) 
                           _ -> error (s ++ " ???")

        
