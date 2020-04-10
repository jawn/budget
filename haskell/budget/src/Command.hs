module Command
    where   
import Data.Char
import Category
import Period

data Command 
    = Summary (Maybe FilePath) (Maybe FilePath)
    | Detail  (Maybe FilePath) (Maybe Category) (Maybe Period)
    | Help
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either String Command
command [] = Left "no command given"
command (cmd:args) 
  | cmd `equals` "summary" = Right $ addParameters (Summary Nothing Nothing) args
  | cmd `equals` "detail" = Right $ addParameters (Detail Nothing Nothing Nothing) args
  | cmd `equals` "help" = Right Help
command (cmd:args) = Left $ "unknown command: "++ unwords (cmd:args)

same :: Eq a =>  (b -> a) -> b -> b -> Bool
same f x y= f x == f y 

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
addParameters (Detail tf ca pe) ("-p":day1:day2:args) = case periodFromStrings day1 day2 of
                                                          Right p -> addParameters (Detail tf ca (Just p)) args
                                                          Left p -> error p

