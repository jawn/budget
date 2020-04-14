module Same
    where

same :: Eq (a) => (b -> a) -> b -> b -> Bool
same f a b = f a == f b
