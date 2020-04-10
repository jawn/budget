
module Period 
    where
import Data.List (sort)
import Data.Time
import Data.Time.Calendar (Day,diffGregorianDurationClip, fromGregorian, gregorianMonthLength, CalendarDiffDays(..))

type Period = (Day, Day)

period :: Day -> Day -> Period
period day1 day2 | day2 < day1 = period day2 day1
period day1 day2 = (day1,day2) 

months :: Period -> Integer
months (begin,end) = 1 + cdMonths (diffGregorianDurationClip end begin)

periodFromStrings :: String -> String -> Either String Period
periodFromStrings s t = period <$> parse s <*> parse t
    where 
        parse :: String -> Either String Day
        parse s = case parseTimeM True defaultTimeLocale "%m/%d/%Y" s :: Maybe Day of
                    Nothing -> Left $ "parse error: wrond date format: " ++ s 
                    Just d -> Right d

periodFromMonth :: Integer -> Int -> Period
periodFromMonth y m  = (fromGregorian y m 1, fromGregorian y m (gregorianMonthLength y m))
