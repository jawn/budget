module Help
    where


help :: IO ()
help = do 
    putStrLn 
    $ unlines 
    [ "usage: budget help"
    , "       budget summary [-t <transactions.csv>] [-c <category-selection.csv>]"
    , "       budget detail  [-t <transaction.csv>] [-c Category] [-p <mm/dd/YYYY> <mm/dd/YYYY>] [-m YYYY mm] [-s AaCcDdMmNnOo]"
    , "       budget import <transaction.csv> Accout"
    ]

