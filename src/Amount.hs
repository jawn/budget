module Amount ( Amount (..)
              , amount
              , divideBy
              , number
              , total )
    where
import Text.Printf
import Data.Csv
import FieldToString

data Amount = Amount Int
    deriving (Eq, Ord)

amount :: Double -> Amount
amount f | f < 0 = negate (amount (abs f)) 
         | otherwise = Amount $ dollars  * 100 + cents 
        where 
            dollars = n `div` 100
            cents   = n `rem` 100
            n = truncate (f * 100)

instance Show Amount
    where 
        show (Amount n) | n < 0 = '-' : show (negate (Amount n))
                        | otherwise = printf "%d.%02d" dollars cents
                            where
                                dollars = n `div` 100
                                cents   = n `rem` 100


instance Read Amount
    where
        readsPrec _ ('-':s) = case reads s :: [(Double,String)] of
                                [] -> []
                                ((d,r):_) -> [(negate (amount d), r)]
        readsPrec _ s = case reads s :: [(Double,String)] of
                            [] -> []
                            ((d,r):_) -> [(amount d, r)]

instance Num Amount
    where
        (+) (Amount n) (Amount m) = Amount (n+m)
        (*) (Amount n) (Amount m) = Amount ((n*m) `div` 100)
        abs (Amount n) = Amount (abs n)
        signum (Amount n) = Amount (signum n)
        negate (Amount n) = Amount (negate n)
        fromInteger n = Amount (fromIntegral n)

number :: Amount -> Int
number (Amount n) = n

total :: [Amount] -> Amount
total = Amount . sum . map number

divideBy :: Amount -> Integer -> Amount
divideBy (Amount n) i = Amount (n `div` fromIntegral i)

instance FromField Amount where
    parseField s | head (fieldToString s) == '-' = fmap negate $ parseField (stringToField (tail (fieldToString s)))
    parseField s = case runParser (parseField s :: Parser Double) of
                     Right n -> pure $ amount n
                     Left msg -> fail msg

instance ToField Amount where
    toField = stringToField . show

