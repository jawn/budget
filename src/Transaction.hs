module Transaction ( Transaction (..)
                   , sameTransaction
                   , withCategoryIn
                   , withinPeriod
    )
    where

import Account
import Amount
import Category
import Date
import Name
import Note
import Period

import Data.Csv
    ( FromRecord(parseRecord)
    , (.!)
    , Parser 
    , toRecord
    , record
    , ToRecord
    , toField
    )

import qualified Data.ByteString.Lazy as ByteString

data Transaction = Transaction { transactionAccount  :: Account
                               , transactionDate     :: Date
                               , transactionNotes    :: Maybe Note
                               , transactionName     :: Maybe Name
                               , transactionCategory :: Category
                               , transactionAmount   :: Amount
                               }
    deriving (Eq, Ord, Show)

sameTransaction :: Transaction -> Transaction -> Bool
sameTransaction t1 t2 = transactionDate t1 == transactionDate t2
                    && transactionName t1 == transactionName t2
                    && transactionAmount t1 == transactionAmount t2

withCategoryIn :: Transaction -> [Category] -> Bool
_ `withCategoryIn` [] = True
t `withCategoryIn` cats = transactionCategory t `elem` cats

withinPeriod :: Transaction -> Period -> Bool
withinPeriod t p = (transactionDate t) `within` p


instance FromRecord Transaction where
    parseRecord v
      | length v <  7  = fail (show v)
      | otherwise = Transaction
                        <$> v .!  0
                        <*> v .!  2
                        <*> (v .!  3 :: Parser (Maybe Note))
                        <*> (v .!  4 :: Parser (Maybe Name))
                        <*> v .!  5
                        <*> v .!  6

instance ToRecord Transaction
    where toRecord t = record [ toField (transactionAccount t)
                              , toField ByteString.empty -- dummy empty field
                              , toField (transactionDate t)
                              , toField (transactionNotes t)
                              , toField (transactionName t)
                              , toField (transactionCategory t)
                              , toField (transactionAmount t)
                              ]



