module Help
    where


help :: IO ()
help = do 
    putStrLn 
    $ unlines 
    [ "usage: budget help"
    , "       budget summary"
    , "       budget summary [-t <my-transactions.csv>] [-c <my-category-selection.csv>]"
    ]

