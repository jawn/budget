module Help ( Topic (..)
            , help
            , topic )
    where
import Command
import Config
import ExitWithMsg
import System.Directory
import Data.Maybe
import Control.Monad
import MaybeToEither

data Topic = TopicHelp | TopicSummary | TopicDetail | TopicImport | TopicSort | TopicConfig
    deriving (Eq, Show)

topic :: String -> Topic
topic args | args `equals` "summary" = TopicSummary
topic args | args `equals` "detail"  = TopicDetail
topic args | args `equals` "import"  = TopicImport
topic args | args `equals` "sort"    = TopicSort
topic args | args `equals` "config"  = TopicConfig
topic _ = TopicHelp

help :: [String] -> IO ()
help [] = doHelp TopicHelp
help args = doHelp (topic (args!!0))

doHelp TopicHelp =
    (putStr . unlines)
                  [ "budget help [command]"
                  , "commands are:"
                  , "   summary"
                  , "   detail"
                  , "   import"
                  , "   sort"
                  , "   config"
                  ]
doHelp TopicSummary =
    (putStr . unlines)
        [ "    budget summary"
        , ""
        , "(or budget Sum or budget s ..)"
        , "will print the total by category for all the transactions in the main transactions csv file."
        , ""
        , "    budget summary -t OtherTransactions.csv"
        , ""
        , "will print the total by category for all the transactions in the alternative file `OtherTransactions.csv`."
        , ""
        , ""
        , "    budget summary -c Categories.csv"
        , ""
        , "will print the total by category for categories given in the selection file `Categories.csv` only."
        , ""
        , "    budget summary -s M"
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
        , ""
        , "will print the transactions in the alternative file 'OtherTransactions.csv'."
        , ""
        , "    budget d -c \"Business Expenses\""
        , ""
        , "will print the transactions having the category 'Business Expenses'."
        , ""
        , "    budget d -p 01/01/2020 03/31/2020"
        , ""
        , "will print the transactions having their date between the January 1st 2020 and the March 31st 2020."
        , ""
        , "    budget d -m 2020 4"
        , ""
        , "will print the transactions having their date in April 2020. "
        , ""
        , "    budget d -s AD"
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
        , "assigning 'Checkingr' as the Account for these transactions. If the transactions have already been"
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
        , "files containing transactions that are already in the main transaction file will no be imported."
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
    cfg <- Config.fromFile $ home ++ "/.budget_conf"
    either putStrLn printConfig cfg

printConfig :: Config -> IO ()
printConfig cfg = do
    mapM_ (\(k,v) -> putStrLn (k ++ ":" ++ v)) cfg

