module Period ( Period (..)
              , months
              , period
              , periodFromMonthString
              , periodFromMonth
              , periodFromStrings
              , periodFromYear
              , periodFromYearString
              , theDay
              , within
              )
    where

import Date
import Message ( Message )

import Text.Printf
import Data.Time ( defaultTimeLocale , parseTimeM)
import Data.Time.Calendar (Day,diffGregorianDurationClip, gregorianMonthLength, CalendarDiffDays(..))

data Period = Period Date Date
    deriving (Eq)

instance Show Period 
    where
        show (Period d1 d2) =  
            printf "from %s to %s" (show d1) (show d2)

period :: Date -> Date -> Period
period day1 day2 | day2 < day1 = period day2 day1
period day1 day2 = Period day1 day2

months :: Period -> Integer
months (Period begin end) = 1 + cdMonths (diffGregorianDurationClip (day end) (day begin))

periodFromStrings :: String -> String -> Either Message Period
periodFromStrings day1 day2 = period <$> parse day1 <*> parse day2
    where 
        parse :: String -> Either Message Date
        parse s = case parseTimeM True defaultTimeLocale "%m/%d/%Y" s :: Maybe Day of
                    Nothing -> Left $ "parse error: wrong date format: " ++ s 
                    Just d -> Right (Date d)

periodFromMonth :: Integer -> Int -> Period
periodFromMonth y m  = Period (theDay y m 1) (theDay y m (gregorianMonthLength y m))

periodFromYear :: Integer -> Period
periodFromYear y = Period (theDay y 1 1) (theDay y 12 31)

periodFromMonthString :: String -> String -> Either Message Period
periodFromMonthString s t  = case reads s of
                               []  -> Left $ "parser error: wrong year format: " ++ s 
                               ((y,_):_) -> case reads t of
                                            [] -> Left $ "parser error: wrong month format: " ++ t
                                            ((m,_):_) -> Right $ periodFromMonth y m

periodFromYearString :: String -> Either Message Period
periodFromYearString s = case reads s of
                           [] -> Left $ "parser error: wrong year format: " ++ s
                           ((y,_):_) -> Right $ periodFromYear y

within :: Date -> Period -> Bool
within d (Period begin end) = d >= begin && d <= end
