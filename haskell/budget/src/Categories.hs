module Categories
    where
import Expense


importCategories :: String -> [Category]
importCategories = map read . lines 
