module Detail 
    where

import Account     ( Account (..) )
import Amount      ( Amount (..) )
import Category    ( Category (..), CategorySelector )
import ExitWithMsg ( exitWithMsg )
import Message     ( Message )
import Period      ( Period
                   , within )
import Sorting     ( SortingCriteria
                   , sortWithCriteria )
import Transaction ( Transaction (..) 
                   , totalTransactions
                   , transactionsPeriod )

import Data.List   ( intercalate )
import Data.Maybe  ( catMaybes )
import Text.Printf ( printf )
import Data.Time   ( Day 
                   , defaultTimeLocale
                   , formatTime
                   )

maybeFilter :: Maybe (Transaction -> Bool) -> [Transaction] -> [Transaction]
maybeFilter Nothing tr = tr
maybeFilter (Just p) tr  = filter p tr
detailLines 
    :: Maybe Category
    -> Maybe Period
    -> SortingCriteria
    -> CategorySelector
    -> [Transaction]
    -> [Transaction]
detailLines cat per sct sel = 
    (sortWithCriteria sct)
        . maybeFilter (fmap (\c -> \t -> ((transactionCategory t)== c)) cat)
        . maybeFilter (fmap (\p -> \t -> ((transactionDate t) `within` p)) per)
        . maybeFilter (fmap (\s -> \t -> (s (transactionCategory t))) (pure sel))
        

prettyLine :: Transaction -> String
prettyLine t = printf "%-20s %10s %-20s %-20s %-40s|%10s"
             (take 20 (accountName (transactionAccount t)))
             (formatTime defaultTimeLocale "%m/%d/%Y" (transactionDate t))
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
    -> Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> String
detailTitle Nothing Nothing Nothing Nothing = "Transactions (all)"
detailTitle fp cf c p = "Transactions " ++ intercalate " " (catMaybes options)
    where
        options = [ fmap ("from file: "++) fp
                  , fmap ("with categories from the file: "++) cf 
                  , fmap (("with category: "++) . categoryName) c
                  , fmap show p
                  ]

detail 
    :: Maybe FilePath
    -> Maybe FilePath
    -> Maybe Category
    -> Maybe Period
    -> SortingCriteria
    -> CategorySelector
    -> [Transaction]
    -> [String]
detail tfp cfp cat per sct sel trs = 
        let selection = detailLines cat per sct sel trs
        in [ detailTitle tfp cfp cat per ]
        ++ (map prettyLine selection)
        ++ (footer tfp selection)
