module Amount ( Amount (..)
              , amount
              , divideBy
              , number
              , total )
    where
import Text.Printf

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
                                [(d,s)] -> [(negate (amount d), s)]
        readsPrec _ s = case reads s :: [(Double,String)] of
                            [] -> []
                            [(d,s)] -> [(amount d, s)]

instance Num Amount
    where
        (+) (Amount n) (Amount m) = Amount (n+m)
        negate (Amount n) = Amount (negate n)
        fromInteger n = Amount (fromIntegral n)

number :: Amount -> Int
number (Amount n) = n

total :: [Amount] -> Amount
total = Amount . sum . map number

divideBy :: Amount -> Integer -> Amount
divideBy (Amount n) i = Amount (n `div` fromIntegral i)
