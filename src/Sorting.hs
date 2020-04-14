module Sorting 
    where

import Amount
import Category
import Account
import Transaction
import Period
import Data.Ord
import Data.List

type SortingCriteria = String
data CommandCriteria = DetailSortingCriteria | SummarySortingCriteria
                     deriving (Eq, Show)

ordering :: Char -> Transaction -> Transaction -> Ordering
ordering 'A' = comparing transactionAccount
ordering 'a' = flip $ comparing transactionAccount
ordering 'C' = comparing transactionCategory
ordering 'c' = flip $ comparing transactionCategory
ordering 'D' = comparing transactionDate
ordering 'd' = flip $ comparing transactionDate
ordering 'M' = comparing transactionAmount 
ordering 'm' = flip $ comparing transactionAmount 
ordering 'N' = comparing transactionName
ordering 'n' = flip $ comparing transactionName
ordering 'O' = comparing transactionNotes
ordering 'o' = flip $ comparing transactionNotes
ordering _ = error "unknown sort criteria"

orderings :: Transaction -> Transaction -> SortingCriteria -> Ordering
orderings t1 t2 = foldOrdering . map (\c -> ordering c t1 t2) 

foldOrdering :: [Ordering] -> Ordering
foldOrdering [] = EQ
foldOrdering (EQ:ords) = foldOrdering ords
foldOrdering (ord:_) = ord

sortWithCriteria :: SortingCriteria -> [Transaction] -> [Transaction]
sortWithCriteria s = sortBy (\t u -> orderings t u s)

validateCriteria :: CommandCriteria -> SortingCriteria -> Either String SortingCriteria
validateCriteria DetailSortingCriteria s | any (not . (`elem` "AaCcDdMmNnOo")) s =
    (Left $ unlines [ "wrong sorting criteria: "++s
                    , "Available criteria are one or many of:"
                    , "A : Account ascending (a : descending)"
                    , "C : Category ascending (c : descending)"
                    , "D : Date ascending (d : descending)"
                    , "M : Amount ascending (m : descending)"
                    , "N : Name ascending (n : descending)"
                    , "O : Notes ascending (o : descending)"
                    ])
validateCriteria DetailSortingCriteria s = Right s
validateCriteria SummarySortingCriteria s | any (not . (`elem` "CcMm")) s || length s > 1=
                    (Left $ unlines [ "wrong sorting criteria: " ++ s
                                    , "Available criteria are one of:"
                                    , "C : Category ascending (c : descending)"
                                    , "M : Amount ascending (m : descending)"
                                    ])
validateCriteria SummarySortingCriteria s = Right s
