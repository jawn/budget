module Command
    where   
import Data.Char

data Command 
    = Summary (Maybe FilePath) (Maybe FilePath)
    | Help
            
    deriving (Eq, Show)

command 
    :: [String]
    -> Either String Command
command [] = Left "no command given"
command (cmd:args) 
  | cmd `equals` "summary" && length args == 0 = Right (Summary Nothing Nothing)
  | cmd `equals` "summary" && length args == 1 = Right (Summary (Just (args!!0)) Nothing)
  | cmd `equals` "summary" && length args == 2 = Right (Summary (Just (args!!0)) (Just (args!!1)))
  | cmd `equals` "help" = Right Help
command (cmd:args) = Left $ "unknown command: "++cmd    

same :: Eq a =>  (b -> a) -> b -> b -> Bool
same f x y= f x == f y 

lowerCase :: String -> String
lowerCase = map toLower 

equals :: String -> String -> Bool
s `equals` t = same (lowerCase . take l) s t 
    where l = length s
