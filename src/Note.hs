module Note ( Note (..) )
    where 

import Data.Csv ( FromField
                , ToField
                , parseField 
                , toField 
                )
import FieldToString

newtype Note = Note String
    deriving (Eq,Ord)

instance Show Note where
    show (Note n) = n

instance FromField Note where
    parseField = pure . Note . fieldToString

instance ToField Note where
    toField (Note n) = stringToField $ n

