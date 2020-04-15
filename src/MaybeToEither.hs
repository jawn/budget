module MaybeToEither ( maybeToEither )
    where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

