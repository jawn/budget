module Name ( Name (..) )
    where

import Data.Csv ( FromField
                , ToField
                , parseField 
                , toField 
                )
import FieldToString

newtype Name = Name String
    deriving (Eq, Ord)

instance Show Name where
    show (Name n) = n

instance FromField Name where
    parseField = pure . Name . fieldToString

instance ToField Name where
    toField (Name n) = stringToField $ n 

