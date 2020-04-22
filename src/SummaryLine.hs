module SummaryLine ( SummaryLine (..) )
    where

import Amount
import Category
import Text.Printf

data SummaryLine = SummaryLine { summaryLineCategory :: Category
                               , summaryLineAmount   :: Amount
                               , summaryLineAverage  :: Amount
                               }
    deriving (Eq, Ord)

instance Show SummaryLine 
    where
        show (SummaryLine cat amt avg) =
            printf "%-49s:%10s |%10s" 
                (categoryName cat) (show amt) (show avg)


