module Period 
    where
import Data.List (sort)

import Data.Time.Calendar (Day,diffGregorianDurationClip, fromGregorian, CalendarDiffDays(..))

type Period = (Day, Day)

months :: Period -> Integer
months (begin,end) = 1 + cdMonths (diffGregorianDurationClip end begin)

