module Name ( Name (..) )
    where

newtype Name = Name String
    deriving (Eq, Ord)

instance Show Name where
    show (Name n) = n
