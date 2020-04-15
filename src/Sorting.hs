module Sorting ( SortingCriteria
               , SortCriterion (..)
               , CommandCriteria (..)
               , readCriteria
               , summaryOrdering
               , validateCriteria
               , sortWithCriteria
               )
    where

import Message
import Amount
import Category
import Account
import Transaction
import SummaryLine

import Period
import Data.Ord
import Data.List
import Data.Either

type SortingCriteria = [SortCriterion]
data Direction = Asc | Desc
    deriving (Eq, Show)

data SortCriterion = 
    AccountAsc
  | AccountDesc
  | CategoryAsc
  | CategoryDesc
  | DateAsc
  | DateDesc
  | AmountAsc
  | AmountDesc
  | NameAsc
  | NameDesc
  | NotesAsc
  | NotesDesc
  deriving (Eq, Show)

data CommandCriteria = DetailSortingCriteria 
                     | SummarySortingCriteria
     deriving (Eq, Show)

readCriteria 
    :: CommandCriteria 
    -> String 
    ->  Either Message [SortCriterion]
readCriteria SummarySortingCriteria s 
    | length s > 1 = Left $ "too many criteria: " ++ s
readCriteria cmd s =
    case (partitionEithers . map (readCriterion cmd)) s of
        ([],criteria) -> Right criteria
        (messages,_)   -> Left $ intercalate "\n" messages 

readCriterion 
    :: CommandCriteria 
    -> Char 
    -> Either Message SortCriterion
readCriterion _ 'C' = Right CategoryAsc 
readCriterion _ 'c' = Right CategoryDesc
readCriterion _ 'M' = Right AmountAsc   
readCriterion _ 'm' = Right AmountDesc  
readCriterion DetailSortingCriteria 'A' = Right AccountAsc  
readCriterion DetailSortingCriteria 'a' = Right AccountDesc 
readCriterion DetailSortingCriteria 'D' = Right DateAsc
readCriterion DetailSortingCriteria 'd' = Right DateDesc
readCriterion DetailSortingCriteria 'N' = Right NameAsc     
readCriterion DetailSortingCriteria 'n' = Right NameDesc    
readCriterion DetailSortingCriteria 'O' = Right NotesAsc     
readCriterion DetailSortingCriteria 'o' = Right NotesDesc    
readCriterion _ c  = Left $ "not a sort criterion: " ++ [c]


detailOrdering 
    :: SortCriterion 
    -> Transaction 
    -> Transaction 
    -> Ordering
detailOrdering AccountAsc   = comparing transactionAccount
detailOrdering CategoryAsc  = comparing transactionCategory
detailOrdering DateAsc      = comparing transactionDate
detailOrdering AmountAsc    = comparing transactionAmount
detailOrdering NameAsc      = comparing transactionName
detailOrdering NotesAsc     = comparing transactionNotes
detailOrdering AccountDesc  = flip $ comparing transactionAccount
detailOrdering CategoryDesc = flip $ comparing transactionCategory
detailOrdering DateDesc     = flip $ comparing transactionDate
detailOrdering AmountDesc   = flip $ comparing transactionAmount
detailOrdering NameDesc     = flip $ comparing transactionName
detailOrdering NotesDesc    = flip $ comparing transactionNotes

summaryOrdering
    :: SortCriterion
    -> SummaryLine
    -> SummaryLine
    -> Ordering
summaryOrdering CategoryAsc  = comparing summaryCategory
summaryOrdering AmountAsc    = comparing summaryAmount
summaryOrdering CategoryDesc = flip $ comparing summaryCategory
summaryOrdering AmountDesc   = flip $ comparing summaryAmount


detailOrderings :: Transaction -> Transaction -> SortingCriteria -> Ordering
detailOrderings t1 t2 = foldOrdering . map (\o -> detailOrdering o t1 t2) 

foldOrdering :: [Ordering] -> Ordering
foldOrdering [] = EQ
foldOrdering (EQ:ords) = foldOrdering ords
foldOrdering (ord:_) = ord

sortWithCriteria :: SortingCriteria -> [Transaction] -> [Transaction]
sortWithCriteria s = sortBy (\t u -> detailOrderings t u s)

validateCriteria :: CommandCriteria -> String -> Either Message SortingCriteria
validateCriteria DetailSortingCriteria s = 
    either
        (Left . (wrongCriteriaMessage DetailSortingCriteria))
        Right
    (readCriteria DetailSortingCriteria s)

validateCriteria SummarySortingCriteria s = case readCriteria SummarySortingCriteria s of
                                              Left s -> Left (wrongCriteriaMessage SummarySortingCriteria s)
                                              Right criteria -> Right criteria

wrongCriteriaMessage :: CommandCriteria -> String -> String
wrongCriteriaMessage DetailSortingCriteria s = unlines
    [ s
    , "Available criteria are one or many of:"
    , "A : Account ascending (a : descending)"
    , "C : Category ascending (c : descending)"
    , "D : Date ascending (d : descending)"
    , "M : Amount ascending (m : descending)"
    , "N : Name ascending (n : descending)"
    , "O : Notes ascending (o : descending)"
    ]
wrongCriteriaMessage SummarySortingCriteria s = unlines
    [ s
    , "Available criteria are one of:"
    , "C : Category ascending (c : descending)"
    , "M : Amount ascending (m : descending)"
    ]

