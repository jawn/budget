module CategorySelection ( CategorySelection (..)
                         , CategorySelector
                         , categorySelection 
                         , excluded
                         , categorySelector
                         )
    where

import Category
import CategoryList
import Domain
import IsCsvFile
import SelectionType
import Transaction

data CategorySelection = AllCategories
                       | SingleCategory     Category SelectionType
                       | CategoriesFromFile FilePath SelectionType
    deriving (Eq)

type CategorySelector = (Transaction -> Bool)

categorySelection :: String -> CategorySelection
categorySelection "" = AllCategories
categorySelection name | isCSVFile name = CategoriesFromFile name Selected
categorySelection name = SingleCategory (Category name) Selected

excluded :: CategorySelection -> CategorySelection
excluded AllCategories = AllCategories
excluded (SingleCategory n _) = SingleCategory n Excluded
excluded (CategoriesFromFile n _) = CategoriesFromFile n Excluded

categorySelector :: CategorySelection -> Domain CategorySelector
categorySelector AllCategories = domain $ Right $ acceptAll
    where
    acceptAll :: CategorySelector
    acceptAll = const True
categorySelector (SingleCategory c Selected) 
  = domain $ Right $ (c ==) . transactionCategory
categorySelector (SingleCategory c Excluded) 
  = domain $ Right $ (c /=) . transactionCategory 
categorySelector (CategoriesFromFile fp Selected)
  = do
      cs <- categoriesFromFile fp
      return $ ((`elem` cs) . transactionCategory)
categorySelector (CategoriesFromFile fp Excluded)
  = do
      cs <- categoriesFromFile fp
      return $ (not . (`elem` cs) . transactionCategory)

instance Show CategorySelection where
    show (AllCategories) = "all categories"
    show (SingleCategory c Selected) = "category: " ++ categoryName c
    show (SingleCategory c Excluded) = "all categories except: " ++ categoryName c
    show (CategoriesFromFile fp Selected) = "categories in the file: " ++ fp
    show (CategoriesFromFile fp Excluded) = "all categories except those in the file: " ++ fp
