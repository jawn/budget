module Command
    where   

import CategorySelection
import Domain
import Message ( Message )
import Period
import Same
import Sorting

import Control.Monad.Except
import Data.Char

data Command 
    = Summary { summaryTransactionFilePath :: Maybe FilePath
              , summaryCategorySelection   :: CategorySelection
              , summaryPeriod              :: Maybe Period
              , summarySortingCriteria     :: SortingCriteria
              }
    | Detail  { detailTransactionFilePath :: Maybe FilePath
              , detailCategorySelection   :: CategorySelection
              , detailPeriod              :: Maybe Period
              , detailSortingCriteria     :: SortingCriteria 
              }
    | Import FilePath (Maybe String)
    | Help  [String]
    | Version
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Domain Command
command [] = command ["summary"]
command (cmd:args) 
  | cmd `equals` "summary" = domain $ (Summary Nothing AllCategories Nothing []) `with` args
  | cmd `equals` "detail"  = domain $ (Detail Nothing AllCategories Nothing [])  `with` args
  | cmd `equals` "import" && length args == 2 = domain $ Right (Import (args!!0) (Just (args!!1)))
  | cmd `equals` "import" && length args == 1 = domain $ Right (Import (args!!0) Nothing)
  | cmd `equals` "import" && length args < 1 = domain $ Left "import: missing argument (import {<filename> <accountname> | <folder> }"
  | cmd `equals` "help" = domain $ Right (Help args)
  | cmd `equals` "version" = domain $ Right Version
command (cmd:args) = throwError $ "unknown command: "++ unwords (cmd:args)


lowerCase :: String -> String
lowerCase = map toLower 

equals :: String -> String -> Bool
s `equals` t = same (lowerCase . take (length s)) s t 

with 
    :: Command 
    -> [String] 
    -> Either Message Command

cmd `with` [] = Right cmd

cmd@( Summary _ _ _ _) `with` ("-t":arg:args)  = 
    cmd { summaryTransactionFilePath = Just arg } `with` args

cmd@( Summary _ _ _ _) `with` ("-c":arg:args) = 
    cmd { summaryCategorySelection = categorySelection arg } `with` args

cmd@( Summary _ _ _ _) `with` ("-x":arg:args) = 
    cmd { summaryCategorySelection = excluded (categorySelection arg) } `with` args

cmd@( Summary _ _ _ _) `with` ("-p":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@( Summary _ _ _ _) `with` ("-m":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with`args)

cmd@( Summary _ _ _ _) `with` ("-y":arg:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromYearString arg >>= (`with`args)

cmd@( Summary _ _ _ _) `with` ("-s":arg:args) = 
    (\c -> cmd { summarySortingCriteria = c }) <$> validateCriteria SummarySortingCriteria arg >>= (`with`args)

cmd@(Summary _ _ _ _) `with` (opt:arg:args) | opt `equals` "category" = cmd `with` ("-c":arg:args)  

cmd@(Detail _ _ _ _) `with` (opt:arg:args) | opt `equals` "category" = cmd `with` ("-c":arg:args)  

cmd `with` (opt:arg:args) | opt `equals` "categories"   = cmd `with` ("-c":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "except"       = cmd `with` ("-x":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "transactions" = cmd `with` ("-t":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "period"       = cmd `with` ("-p":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "month"        = cmd `with` ("-m":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "year"         = cmd `with` ("-y":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "sortby"       = cmd `with` ("-s":arg:args) 
                                    
cmd@(Detail _ _ _ _) `with` ("-t":arg:args) =
    cmd { detailTransactionFilePath = Just arg } `with` args

cmd@(Detail _ _ _ _) `with` ("-c":arg:args) = 
    cmd { detailCategorySelection = categorySelection arg } `with` args

cmd@(Detail _ _ _ _) `with` ("-x":arg:args) = 
    cmd { detailCategorySelection = excluded (categorySelection arg) } `with` args

cmd@(Detail _ _ _ _) `with` ("-p":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@(Detail _ _ _ _) `with` ("-m":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with` args)

cmd@(Detail _ _ _ _) `with` ("-y":arg:args) =  
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromYearString arg >>= (`with` args)

cmd@(Detail _ _ _ _) `with` ("-s":arg:args) = 
    (\c -> cmd { detailSortingCriteria = c }) <$> validateCriteria DetailSortingCriteria arg >>= (`with` args)


_ `with` (arg:_) = Left ("option unrecognized or incomplete: " ++ arg)


