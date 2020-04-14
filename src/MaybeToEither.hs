module MaybeToEither
    where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

