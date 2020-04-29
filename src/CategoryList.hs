module CategoryList ( CategoryList
                    , categoriesFromFile
    )
    where

import Category
import Domain

import Data.Csv
    ( HasHeader(NoHeader)
    , decode
    )
import qualified Data.Vector as Vector (toList)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

type CategoryList = [Category]


categoriesFromFile :: FilePath -> Domain CategoryList
categoriesFromFile filePath = do
    content <- catchIODomain (ByteString.readFile filePath)
    categories <- categoriesFromByteString content
    return categories

categoriesFromByteString :: ByteString -> Domain CategoryList 
categoriesFromByteString =
    domain . fmap Vector.toList . decode NoHeader
