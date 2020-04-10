module Account
    where

data Account = Account { accountName :: String }
    deriving (Eq,Ord,Show)

