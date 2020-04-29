module Command
    where   

import Category
import Domain
import Message ( Message )
import Period
import Same
import Sorting

import Control.Monad.Except
import Data.Char
import System.FilePath.Posix (takeExtension)

data Command 
    = Summary { summaryTransactionFilePath :: Maybe FilePath
              , summaryCategoriesFilePath  :: Maybe FilePath
              , summaryCategory            :: Maybe Category
              , summaryPeriod              :: Maybe Period
              , summarySortingCriteria     :: SortingCriteria
              }
    | Detail  { detailTransactionFilePath :: Maybe FilePath
              , detailCategoriesFilePath  :: Maybe FilePath
              , detailCategory            :: Maybe Category
              , detailPeriod              :: Maybe Period
              , detailSortingCriteria     :: SortingCriteria 
              }
    | Import FilePath (Maybe String)
    | Help  [String]
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Domain Command
command [] = command ["summary"]
command (cmd:args) 
  | cmd `equals` "summary" = domain $ (Summary Nothing Nothing Nothing Nothing []) `with` args
  | cmd `equals` "detail"  = domain $ (Detail Nothing Nothing Nothing Nothing [])  `with` args
  | cmd `equals` "import" && length args == 2 = domain $ Right (Import (args!!0) (Just (args!!1)))
  | cmd `equals` "import" && length args == 1 = domain $ Right (Import (args!!0) Nothing)
  | cmd `equals` "import" && length args < 1 = domain $ Left "import: missing argument (import {<filename> <accountname> | <folder> }"
  | cmd `equals` "help" = domain $ Right (Help args)
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

cmd@( Summary _ _ _ _ _) `with` ("-t":arg:args)  = 
    cmd { summaryTransactionFilePath = Just arg } `with` args

cmd@(Summary _ _ _ _ _) `with` ("-c":arg:args) = 
    cmd `withCategoryOption` arg `with` args
    where 
        withCategoryOption :: Command -> String -> Command
        c `withCategoryOption` name | isCSVFile name = c { summaryCategoriesFilePath = Just name  } 
                                    | otherwise      = c { summaryCategory = Just (Category name) } 

cmd@( Summary _ _ _ _ _) `with` ("-p":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@( Summary _ _ _ _ _) `with` ("-m":arg1:arg2:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with`args)

cmd@( Summary _ _ _ _ _) `with` ("-y":arg:args)  = 
    (\p -> cmd { summaryPeriod = Just p }) <$> periodFromYearString arg >>= (`with`args)

cmd@( Summary _ _ _ _ _) `with` ("-s":arg:args) = 
    (\c -> cmd { summarySortingCriteria = c }) <$> validateCriteria SummarySortingCriteria arg >>= (`with`args)

cmd@(Summary _ _ _ _ _) `with` (opt:arg:args) | opt `equals` "category" = cmd `with` ("-c":arg:args)  

cmd@(Detail _ _ _ _ _) `with` (opt:arg:args) | opt `equals` "category" = cmd `with` ("-c":arg:args)  

cmd `with` (opt:arg:args) | opt `equals` "categories"   = cmd `with` ("-c":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "transactions" = cmd `with` ("-t":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "period"       = cmd `with` ("-p":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "month"        = cmd `with` ("-m":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "year"         = cmd `with` ("-y":arg:args) 
cmd `with` (opt:arg:args) | opt `equals` "sortby"       = cmd `with` ("-s":arg:args) 
                                    
cmd@(Detail _ _ _ _ _) `with` ("-t":arg:args) =
    cmd { detailTransactionFilePath = Just arg } `with` args

cmd@(Detail _ _ _ _ _) `with` ("-c":arg:args) = 
    cmd `withCategoryOption` arg `with` args
    where 
        withCategoryOption :: Command -> String -> Command
        c `withCategoryOption` name | isCSVFile name = c { detailCategoriesFilePath = Just name  } 
                                    | otherwise      = c { detailCategory = Just (Category name) } 

cmd@(Detail _ _ _ _ _) `with` ("-p":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromStrings arg1 arg2 >>= (`with` args)

cmd@(Detail _ _ _ _ _) `with` ("-m":arg1:arg2:args) = 
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromMonthString arg1 arg2 >>= (`with` args)

cmd@(Detail _ _ _ _ _) `with` ("-y":arg:args) =  
    (\p -> cmd { detailPeriod = Just p }) <$> periodFromYearString arg >>= (`with` args)

cmd@(Detail _ _ _ _ _) `with` ("-s":arg:args) = 
    (\c -> cmd { detailSortingCriteria = c }) <$> validateCriteria DetailSortingCriteria arg >>= (`with` args)


_ `with` (arg:_) = Left ("option unrecognized or incomplete: " ++ arg)


isCSVFile :: String -> Bool
isCSVFile s = (map toLower (takeExtension s)) == ".csv" 

