module TransactionList ( transactionsFromFile
                       , transactionsToFile
                       )
    where

import Domain
import Transaction

import Data.Csv
    ( HasHeader(NoHeader)
    , decode
    , defaultEncodeOptions
    , encodeWith
    , encUseCrLf
    )



import qualified Data.Vector as Vector (toList)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString


type TransactionList = [Transaction]

transactionsFromFile :: FilePath -> Domain TransactionList
transactionsFromFile filePath = do
    content <- catchIODomain (ByteString.readFile filePath) 
    transactions <- transactionsFromByteString content
    return transactions

transactionsFromByteString :: ByteString -> Domain TransactionList
transactionsFromByteString = 
    domain . fmap Vector.toList . decode NoHeader

transactionsToFile :: FilePath -> TransactionList -> Domain ()
transactionsToFile filePath = 
   catchIODomain . (ByteString.writeFile filePath) . transactionsToByteString 

transactionsToByteString :: TransactionList -> ByteString
transactionsToByteString =
   encodeWith defaultEncodeOptions { encUseCrLf = False }

