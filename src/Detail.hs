module Detail 
    where

import Account     ( Account (..) )
import Amount      ( Amount (..) )
import Category    ( Category (..) )
import CategorySelection
import Period      ( Period )
import Sorting     ( SortingCriteria
                   , sortWithCriteria )
import Transaction ( Transaction (..) 
                   , withinPeriod
                   )
import TransactionList ( transactionsPeriod
                       , totalTransactions
                       )

import Data.List   ( intercalate )
import Data.Maybe  ( catMaybes )
import Text.Printf ( printf )
import Data.Time   ( Day 
                   , defaultTimeLocale
                   , formatTime
                   )

detailLines 
    :: Maybe Period
    -> SortingCriteria
    -> CategorySelector
    -> [Transaction]
    -> [Transaction]
detailLines per sct sel = 
    (sortWithCriteria sct)
        . maybe id (\p -> filter (`withinPeriod` p)) per
        . filter sel
        

prettyLine :: Transaction -> String
prettyLine t = printf "%-20s %10s %-20s %-20s %-40s|%10s"
             (take 20 (accountName (transactionAccount t)))
             (show (transactionDate t))
             (take 20 (maybe "" show (transactionNotes t)))
             (take 20 (maybe "" show (transactionName t)))
             (take 40 (categoryName (transactionCategory t)))
             (show (transactionAmount t))

totalLabel :: Period -> Amount -> String
totalLabel p a = printf "%-114s:%10s" ("TOTAL "++ (show p)) (show a) 

formatDate :: Day -> String
formatDate day = formatTime defaultTimeLocale "%m/%d/%Y" day

lengthLabel :: Int -> String
lengthLabel n = printf "%d transactions" n

footer 
    :: Maybe FilePath
    -> [Transaction]
    -> [String]
footer fp [] = ["no transactions", maybe "main transaction file" id fp]
footer fp ts =  [ totalLabel (transactionsPeriod ts) (totalTransactions ts) 
             , lengthLabel (length ts)

             , maybe "main transaction file" id fp
             ]


detailTitle 
    :: Maybe FilePath
    -> CategorySelection
    -> Maybe Period
    -> String
detailTitle Nothing AllCategories Nothing = "Transactions (all)"
detailTitle fp catSel p = "Transactions " ++ intercalate " " (catMaybes options)
    where
        options = [ fmap ("from file: "++) fp
                  , Just $ "for " ++ show catSel
                  , fmap show p
                  ]

detail 
    :: Maybe FilePath
    -> CategorySelection
    -> Maybe Period
    -> SortingCriteria
    -> CategorySelector
    -> [Transaction]
    -> [String]
detail tfp catSel per sct sel trs = 
        let selection = detailLines per sct sel trs
        in [ detailTitle tfp catSel per ]
        ++ (map prettyLine selection)
        ++ (footer tfp selection)
