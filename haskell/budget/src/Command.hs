module Command
    where   
import Data.Char
import Category
import Period
import Same

data Command 
    = Summary (Maybe FilePath) (Maybe FilePath)
    | Detail  (Maybe FilePath) (Maybe Category) (Maybe Period) 
    | Import FilePath String
    | Help
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either String Command
command [] = Right (Summary Nothing Nothing)
command (cmd:args) 
  | cmd `equals` "summary" = Right $ addParameters (Summary Nothing Nothing) args
  | cmd `equals` "detail" = Right $ addParameters (Detail Nothing Nothing Nothing) args
  | cmd `equals` "import" && length args == 2 = Right (Import (args!!0) (args!!1))
  | cmd `equals` "import" && length args < 2 = Left "import: missing argument (import <filename> <accountname>)" 
  | cmd `equals` "help" = Right Help
command (cmd:args) = Left $ "unknown command: "++ unwords (cmd:args)

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
addParameters (Detail tf ca pe) ("-t":arg:args) = addParameters (Detail (Just arg) ca pe) args
addParameters (Detail tf ca pe) ("-c":arg:args) = addParameters (Detail tf (Just (Category arg)) pe) args
addParameters (Detail tf ca pe) ("-p":arg1:arg2:args) = case periodFromStrings arg1 arg2 of
                                                          Right p -> addParameters (Detail tf ca (Just p)) args
                                                          Left msg -> error msg
addParameters (Detail tf ca pe) ("-m":arg1:arg2:args) = case periodFromMonthString arg1 arg2 of
                                                          Right p -> addParameters (Detail tf ca (Just p)) args
                                                          Left msg -> error msg

