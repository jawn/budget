module Command
    where   

import Message ( Message )
import Data.Char
import Category
import Period
import Same
import Sorting

data Command 
    = Summary (Maybe FilePath) (Maybe FilePath) SortingCriteria
    | Detail  (Maybe FilePath) (Maybe Category) (Maybe Period) SortingCriteria
    | Import FilePath (Maybe String)
    | Help  [String]
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either Message Command
command [] = Right (Summary Nothing Nothing [])
command (cmd:args) 
  | cmd `equals` "summary" = Right (Summary Nothing Nothing []) >>= addParameters args 
  | cmd `equals` "detail"  = Right (Detail Nothing Nothing Nothing []) >>= addParameters args
  | cmd `equals` "import" && length args == 2 = Right (Import (args!!0) (Just (args!!1)))
  | cmd `equals` "import" && length args == 1 = Right (Import (args!!0) Nothing)
  | cmd `equals` "import" && length args < 1 = Left "import: missing argument (import {<filename> <accountname> | <folder> }"
  | cmd `equals` "help" = Right (Help args)
command (cmd:args) = Left $ "unknown command: "++ unwords (cmd:args)


lowerCase :: String -> String
lowerCase = map toLower 

equals :: String -> String -> Bool
s `equals` t = same (lowerCase . take l) s t 
    where l = length s

addParameters 
    :: [String]
    -> Command
    -> Either Message Command
addParameters [] cmd = Right cmd
addParameters ("-t":arg:args)       (Summary tf sf sc)   = Right (Summary (Just arg) sf sc) >>= addParameters args 
addParameters ("-c":arg:args)       (Summary tf sf sc)   = Right (Summary tf (Just arg) sc) >>= addParameters args
addParameters ("-s":arg:args)       (Summary tf sf sc)   = case validateCriteria SummarySortingCriteria arg of
                                                             Right criteria -> Right (Summary tf sf criteria) >>= addParameters args
                                                             Left msg -> Left msg
addParameters (opt:arg:args) (Summary tf sf sc)  | opt `equals` "categories"   = addParameters ("-c":arg:args) (Summary tf sf sc)
addParameters (opt:arg:args) (Summary tf sf sc)  | opt `equals` "transactions" = addParameters ("-t":arg:args) (Summary tf sf sc)
addParameters (opt:arg:args) (Summary tf sf sc)  | opt `equals` "sortby"       = addParameters ("-s":arg:args) (Summary tf sf sc)
                                    
addParameters ("-t":arg:args)       (Detail tf ca pe sc) = Right (Detail (Just arg) ca pe sc) >>= addParameters args 
addParameters ("-c":arg:args)       (Detail tf ca pe sc) = Right (Detail tf (Just (Category arg)) pe sc) >>= addParameters args
addParameters ("-p":arg1:arg2:args) (Detail tf ca pe sc) = case periodFromStrings arg1 arg2 of
                                                              Right p -> Right (Detail tf ca (Just p) sc) >>= addParameters args
                                                              Left msg -> Left msg
addParameters ("-m":arg1:arg2:args) (Detail tf ca pe sc) = case periodFromMonthString arg1 arg2 of
                                                              Right p -> Right (Detail tf ca (Just p) sc) >>= addParameters args
                                                              Left msg -> Left msg
addParameters ("-s":arg:args)       (Detail tf ca pe sc) = case validateCriteria DetailSortingCriteria arg of 
                                                             Right criteria -> Right (Detail tf ca pe criteria) >>= addParameters args
                                                             Left msg -> Left msg

addParameters (opt:arg:args) (Detail tf ca pe sc) | opt `equals` "transactions" = addParameters ("-t":arg:args) (Detail tf ca pe sc) 
addParameters (opt:arg:args) (Detail tf ca pe sc) | opt `equals` "category" = addParameters ("-c":arg:args) (Detail tf ca pe sc) 
addParameters (opt:arg:args) (Detail tf ca pe sc) | opt `equals` "period" = addParameters ("-p":arg:args) (Detail tf ca pe sc) 
addParameters (opt:arg:args) (Detail tf ca pe sc) | opt `equals` "month" = addParameters ("-m":arg:args) (Detail tf ca pe sc) 
addParameters (opt:arg:args) (Detail tf ca pe sc) | opt `equals` "sortby" = addParameters ("-s":arg:args) (Detail tf ca pe sc) 

addParameters (arg:_)               _                    = Left ("option unrecognized or incomplete: " ++ arg)


