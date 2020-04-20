module Category ( Category (..)
                , CategorySelector )
    where

data Category = Category { categoryName :: String }
    deriving (Eq, Show, Ord)

type CategorySelector = (Category -> Bool)
