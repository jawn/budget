module Categories
    where
import Expense


importCategoriesFromList :: String -> [Category]
importCategoriesFromList = map read . lines 
