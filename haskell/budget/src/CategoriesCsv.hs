module CategoriesCsv
    where

import Category

import CatchShowIO

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Vector (Vector)
import qualified Data.Vector as Vector 

import Data.Csv
    ( FromRecord(parseRecord)
    , (.!)
    , HasHeader(NoHeader)
    , decode
    )

import Control.Monad
import qualified Control.Monad as Monad (mzero)

instance FromRecord Category where
    parseRecord v 
        | length v == 1 = Category <$> v .! 0
        | otherwise = mzero

decodeCategories
    :: ByteString
    -> Either String [Category]
decodeCategories = 
    fmap Vector.toList . decode NoHeader

decodeCategoriesFromFile 
    :: FilePath
    -> IO (Either String [Category])
decodeCategoriesFromFile filePath = 
    catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeCategories

importCategorySelector 
    :: (Maybe FilePath) 
    -> IO (Either String (Category -> Bool))
importCategorySelector Nothing = return $ pure (const True)
importCategorySelector (Just filePath) = do
    let categories = decodeCategoriesFromFile filePath
    fmap (fmap (flip elem)) categories
