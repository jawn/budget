module SummaryLine ( SummaryLine (..)
                   , summaryAmount
                   , summaryCategory
                   )
    where

import Amount
import Category

type SummaryLine = (Category, Amount, Amount)

summaryCategory (c, _, _) = c
summaryAmount   (_, a, _) = a
summaryAverage  (_, _, v) = v

