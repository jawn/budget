module Category ( Category (..) )
    where

data Category = Category { categoryName :: String }
    deriving (Eq, Show, Ord)
