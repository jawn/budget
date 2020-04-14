module Command
    where   
import Data.Char
import Category
import Period
import Same
import Sorting

data Command 
    = Summary (Maybe FilePath) (Maybe FilePath)
    | Detail  (Maybe FilePath) (Maybe Category) (Maybe Period) (Maybe SortingCriteria)
    | Import FilePath (Maybe String)
    | Help  [String]
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either String Command
command [] = Right (Summary Nothing Nothing)
command (cmd:args) 
  | cmd `equals` "summary" = Right $ addParameters (Summary Nothing Nothing) args
  | cmd `equals` "detail" = (Right $ addParameters (Detail Nothing Nothing Nothing Nothing) args) >>= validateDetailSortCriteria
  | cmd `equals` "import" && length args == 2 = Right (Import (args!!0) (Just (args!!1)))
  | cmd `equals` "import" && length args == 1 = Right (Import (args!!0) Nothing)
  | cmd `equals` "import" && length args < 1 = Left "import: missing argument (import {<filename> <accountname> | <folder> }"
  | cmd `equals` "help" = Right (Help args)
command (cmd:args) = Left $ "unknown command: "++ unwords (cmd:args)

validateDetailSortCriteria :: Command -> Either String Command
validateDetailSortCriteria (Detail tf ca pe sc) = case validateCriteria (maybe "D" id sc) of
                                                        Left msg -> Left msg
                                                        other -> Right (Detail tf ca pe sc)

lowerCase :: String -> String
lowerCase = map toLower 

equals :: String -> String -> Bool
s `equals` t = same (lowerCase . take l) s t 
    where l = length s

addParameters 
    :: Command
    -> [String]
    ->  Command
addParameters cmd [] = 
    cmd
addParameters (Summary tf sf)   ("-t":arg:args) = addParameters (Summary (Just arg) sf) args 
addParameters (Summary tf sf)   ("-c":arg:args) = addParameters (Summary tf (Just arg)) args
addParameters (Detail tf ca pe sc) ("-t":arg:args) = addParameters (Detail (Just arg) ca pe sc) args 
addParameters (Detail tf ca pe sc) ("-s":arg:args) = addParameters (Detail tf ca pe (Just arg)) args
addParameters (Detail tf ca pe sc) ("-c":arg:args) = addParameters (Detail tf (Just (Category arg)) pe sc) args
addParameters (Detail tf ca pe sc) ("-p":arg1:arg2:args) = case periodFromStrings arg1 arg2 of
                                                              Right p -> addParameters (Detail tf ca (Just p) sc) args
                                                              Left msg -> error msg
addParameters (Detail tf ca pe sc) ("-m":arg1:arg2:args) = case periodFromMonthString arg1 arg2 of
                                                              Right p -> addParameters (Detail tf ca (Just p) sc) args
                                                              Left msg -> error msg

