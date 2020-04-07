module Categories
    where
import Expense

importCategoriesFromList :: String -> Either String [Category]
importCategoriesFromList = (>>= readCategories) .  Right . lines 
    where
        readCategories :: [String] -> Either String [Category]
        readCategories = sequence  . map readCategory
        readCategory s | '"' `elem` s = case reads s :: [(String,String)] of
                                   [(s,_)] -> Right s
                                   [] -> Left $ "error while reading categories: "++s

        readCategory s = Right s


