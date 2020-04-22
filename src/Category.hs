module Category ( Category (..)
                , decodeCategories
                , decodeCategoriesFromFile
                )
    where

import Message
import CatchShowIO
import FieldToString
import Data.Csv


import qualified Data.Vector as Vector 

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Control.Monad

data Category = Category { categoryName :: String }
    deriving (Eq, Show, Ord)

instance FromRecord Category where
    parseRecord v 
        | length v == 1 = Category <$> v .! 0
        | otherwise = mzero

instance FromField Category where
    parseField = pure . Category . fieldToString

instance ToField Category where
    toField = stringToField . categoryName

decodeCategories
    :: ByteString
    -> Either Message [Category]
decodeCategories = 
    fmap Vector.toList . decode NoHeader

decodeCategoriesFromFile 
    :: FilePath
    -> IO (Either Message [Category])
decodeCategoriesFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeCategories

