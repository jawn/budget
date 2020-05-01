module ImportFileName ( extractAccountNamePart 
                      , importDirectory )
    where

import Message ( Message )

import Data.Char
import System.Directory
import System.FilePath.Posix
import Data.List
import Control.Monad

extractAccountNamePart :: FilePath -> Either Message String
extractAccountNamePart fp = case accountNamePart fp of
                   "" -> Left $ "the file " ++ fp ++ " doesn't contain an account name"
                   n -> Right n

accountNamePart :: FilePath -> String
accountNamePart = filter (not . isDigit) . takeBaseName 

importDirectory :: FilePath -> IO (Either Message [FilePath])
importDirectory fp = do
    contents <- directoryContents fp 
    let filePaths = sort $ map (\fileName -> (fp </> fileName )) contents
    return $ (Right filePaths) >>= checkNotEmpty fp

checkNotEmpty :: FilePath -> [FilePath] ->  Either Message [FilePath]
checkNotEmpty fp [] = Left $ "the directory " ++ fp ++ " doesn't contain import files"
checkNotEmpty _ fs  = Right $ fs

directoryContents :: FilePath -> IO [FilePath]
directoryContents = liftM ((filter notDots) . (filter csvs)) . getDirectoryContents
    where 
        notDots p = head p /= '.'
        csvs p  = map toLower (takeExtension p) == ".csv"


            
