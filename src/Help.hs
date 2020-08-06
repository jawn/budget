module Help ( Topic (..)
            , help
            , topic )
    where
import Command
import Configuration

import System.Directory
import Control.Monad.Except

data Topic = TopicHelp | TopicSummary | TopicDetail | TopicImport | TopicSort | TopicConfig | TopicVersion
    deriving (Eq, Show)

topic :: String -> Topic
topic args | args `equals` "summary" = TopicSummary
topic args | args `equals` "detail"  = TopicDetail
topic args | args `equals` "import"  = TopicImport
topic args | args `equals` "sort"    = TopicSort
topic args | args `equals` "config"  = TopicConfig
topic args | args `equals` "version" = TopicVersion
topic _ = TopicHelp

help :: [String] -> IO ()
help [] = doHelp TopicHelp
help args = doHelp (topic (args!!0))

doHelp :: Topic -> IO () 
doHelp TopicHelp =
    (putStr . unlines)
                  [ "budget help [command]"
                  , "commands are:"
                  , "   summary"
                  , "   detail"
                  , "   import"
                  , "   sort"
                  , "   config"
                  , "   version"
                  ]
doHelp TopicSummary =
    (putStr . unlines)
        [ "    budget summary"
        , ""
        , "(or budget Sum or budget s ..)"
        , "will print the total by category for all the transactions in the main transactions csv file."
        , ""
        , "    budget summary -t OtherTransactions.csv"
        , "    budget summary transactions OtherTransactions.csv"
        , "    budget summary trans OtherTransactions.csv"
        , ""
        , "will print the total by category for all the transactions in the alternative file `OtherTransactions.csv`."
        , ""
        , ""
        , "    budget summary -c Categories.csv"
        , "    budget summary categories Categories.csv"
        , "    budget summary cat Categories.csv"
        , ""
        , "will print the total by category for categories given in the selection file `Categories.csv` only."
        , ""
        , ""
        , "    budget summary -x Categories.csv"
        , "    budget summary except Categories.csv"
        , "    budget summary ex Categories.csv"
        , ""
        , "will print the total by category for all categories except those in the selection file `Categories.csv`."
        , ""
        , "    budget summary -p 01/01/2020 03/31/2020"
        , "    budget sum period 01/01/2020 03/31/2020"
        , "    budget s per 01/01/2020 03/31/2020"
        , ""
        , "will print the total by category for transactions having their date between the January 1st 2020 and the March 31st 2020."
        , ""
        , "    budget summary -m 2020 4"
        , "    budget sum month 2020 4"
        , "    budget s mon 2020 4"
        , ""
        , "will print the total by category for the transactions having their date in 2020. "
        , ""
        , "    budget summary -y 2020"
        , "    budget sum year 2020"
        , "    budget s y 2020"
        , ""
        , "will print the total by category for the transactions having their date in April 2020. "
        , ""
        , "    budget summary -s M"
        , "    budget summary sortby M"
        , ""
        , "will sort the summary  by amount, ascending."
        , "sort criteria are one of:"
        , "    - C : category (c : descending)"
        , "    - M : amount   (m : descending)"
        ]

doHelp TopicDetail =
    (putStr . unlines)
        [ "    budget detail"
        , ""
        , "(or budget d ..)"
        , ""
        , "will print the transactions in the main transactions file."
        , ""
        , "    budget d -t OtherTransactions.csv"
        , "    budget d transactions OtherTransactions.csv"
        , "    budget d tra OtherTransactions.csv"
        , ""
        , "will print the transactions in the alternative file 'OtherTransactions.csv'."
        , ""
        , "    budget d -c \"Business Expenses\""
        , "    budget d category \"Business Expenses\""
        , "    budget d cat \"Business Expenses\""
        , ""
        , "will print the transactions having the category 'Business Expenses'."
        , ""
        , "    budget d -x \"Business Expenses\""
        , "    budget d except \"Business Expenses\""
        , "    budget d ex \"Business Expenses\""
        , ""
        , "will print the transactions having another category than 'Business Expenses'."
        , ""
        , "    budget d -p 01/01/2020 03/31/2020"
        , "    budget d period 01/01/2020 03/31/2020"
        , "    budget d per 01/01/2020 03/31/2020"
        , ""
        , "will print the transactions having their date between the January 1st 2020 and the March 31st 2020."
        , ""
        , "    budget d -m 2020 4"
        , "    budget d month 2020 4"
        , "    budget d mon 2020 4"
        , ""
        , "will print the transactions having their date in April 2020. "
        , ""
        , "    budget d -y 2020"
        , "    budget d year 2020"
        , "    budget d y 2020"
        , ""
        , "will print the transactions having their date in 2020. "
        , ""
        , "    budget d -s AD"
        , "    budget d sortby AD"
        , ""
        , "will print the transactions sorted by Account, then Date"
        , ""
        , "    help sort"
        , ""
        , "will print possible sort criteria"
        ]

doHelp TopicImport =
    (putStr . unlines)
        [ "    budget import MyTransactions.csv Checking"
        , ""
        , "will import the transactions from the file MyTransactions.csv into the main transactions file, "
        , "assigning 'Checking' as the Account for these transactions. If the transactions have already been"
        , "imported (based on the same Name, Date, and Amount) the import will be cancelled."
        , ""
        , "Any transaction with a status different from 'posted' will not be imported."
        , ""
        , "    budget import Chechking202003"
        , ""
        , "will import the transactions from the file Savings202003.csv, assigning 'Savings' as the Account"
        , "for these transactions."
        , ""
        , "    budget import /Users/me/data/downloads"
        , ""
        , "will import all the transactions files from the 'downloads' folder, assigning Account names"
        , "according to filenames."
        , ""
        , "files containing transactions that are already in the main transaction file will not be imported."
        , ""
        , "transactions that are duplicates (same Name, Date and Amount) in the import file will not be imported."
        ]

doHelp TopicSort = 
    (putStr . unlines)
        [ "    budget d -s Cm"
        , ""
        , "will print the transactions sorted by Category (ascending) then Amount (descending) "
        , ""
        , "Sort criteria (they can all be mixed) :"
        , ""
        , "- A : Account (asc)"
        , "- a : Account (desc)"
        , "- C : Category (asc)"
        , "- c : Category (desc)"
        , "- D : Date (asc)"
        , "- d : Date (desc)"
        , "- M : Amount (asc)"
        , "- m : Amount (desc)"
        , "- N : Name (asc)"
        , "- n : Name (desc)"
        , "- O : Note (asc)"
        , "- o : Note (desc)"
        ]


doHelp TopicConfig = do
    (putStr . unlines)
        [ "the file `/User/you/.budget_conf` (on your home directory) should contain the file path of "
        , "your main transaction file. "
        , "(here `you` is your user name; to know your user name, type `whoami`)"
        , ""
        , "the configuration file should define a variable transactions. "
        , "for instance:"
        , ""
        , "    transactions:/Users/you/Documents/MyData/Budget/Transactions.csv"
        , ""
        , "will indicate to every command that the default transaction file to look for is this file."
        ]

    home <- getHomeDirectory
    cfg <- runExceptT $ Configuration.fromFile $ home ++ "/.budget_conf"
    either putStrLn printConfig cfg

doHelp TopicVersion = do
    (putStr . unlines)
        [ "    budget version"
        , ""
        , "will print the version of the application, using the scheme Major.minor.patch"
        ]

printConfig :: Configuration -> IO ()
printConfig cfg = do
    mapM_ (\(k,v) -> putStrLn (k ++ ":" ++ v)) cfg


