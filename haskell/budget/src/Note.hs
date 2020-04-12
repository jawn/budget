module Note where

newtype Note = Note String
    deriving (Eq,Ord)

instance Show Note where
    show (Note n) = n
