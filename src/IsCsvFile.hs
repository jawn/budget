module IsCsvFile ( isCSVFile )
    where

import Data.Char ( toLower )
import System.FilePath.Posix (takeExtension)

isCSVFile :: String -> Bool
isCSVFile s = (map toLower (takeExtension s)) == ".csv" 

