module Command
    where   

import Message ( Message )
import Data.Char
import Category
import Period
import Same
import Sorting

data Command 
    = Summary { summaryTransactionFilePath :: Maybe FilePath
              , summaryCategoriesFilePath  :: Maybe FilePath
              , summaryPeriod              :: Maybe Period
              , summarySortingCriteria     :: SortingCriteria
              }
    | Detail  { detailTransactionFilePath :: Maybe FilePath
              , detailCategory            :: Maybe Category
              , detailPeriod              :: Maybe Period
              , detailSortingCriteria     :: SortingCriteria 
              }
    | Import FilePath (Maybe String)
    | Help  [String]
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either Message Command
command [] = Right (Summary Nothing Nothing Nothing [])
command (cmd:args) 
  | cmd `equals` "summary" = (Summary Nothing Nothing Nothing []) `with` args
  | cmd `equals` "detail"  = (Detail Nothing Nothing Nothing [])  `with` args
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

with :: Command -> [String] -> Either Message Command

cmd `with` [] = Right cmd

cmd@( Summary _ _ _ _) `with` ("-t":arg:args)  = 
    cmd { summaryTransactionFilePath = Just arg } `with` args

cmd@( Summary _ _ _ _) `with` ("-c":arg:args)  = 
    cmd { summaryCategoriesFilePath  = Just arg } `with` args 

cmd@( Summary _ _ _ _) `with` ("-p":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@( Summary _ _ _ _) `with` ("-m":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with`args)

cmd@( Summary _ _ _ _) `with` ("-y":arg:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromYearString arg >>= (`with`args)

cmd@( Summary _ _ _ _) `with` ("-s":arg:args) = 
    (\c -> cmd { summarySortingCriteria = c }) <$> validateCriteria SummarySortingCriteria arg >>= (`with`args)

cmd@(Detail _ _ _ _) `with` (opt:arg:args) | opt `equals` "category" = cmd `with` ("-c":arg:args)  
cmd `with` (opt:arg:args) | opt `equals` "categories"   = cmd `with` ("-c":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "transactions" = cmd `with` ("-t":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "period"       = cmd `with` ("-p":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "month"        = cmd `with` ("-m":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "year"         = cmd `with` ("-y":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "sortby"       = cmd `with` ("-s":arg:args) 
                                    
cmd@(Detail _ _ _ _) `with` ("-t":arg:args) =
    cmd { detailTransactionFilePath = Just arg } `with` args

cmd@(Detail tf ca pe sc) `with` ("-c":arg:args) = 
    cmd { detailCategory = Just (Category arg) } `with` args

cmd@(Detail tf ca pe sc) `with` ("-p":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@(Detail tf ca pe sc) `with` ("-m":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with` args)

cmd@(Detail tf ca pe sc) `with` ("-y":arg:args) =  
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromYearString arg >>= (`with` args)

cmd@(Detail tf ca pe sc) `with` ("-s":arg:args) = 
    (\c -> cmd { detailSortingCriteria = c }) <$> validateCriteria DetailSortingCriteria arg >>= (`with` args)


_ `with` (arg:_) = Left ("option unrecognized or incomplete: " ++ arg)


