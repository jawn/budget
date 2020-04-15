module ImportFileName ( extractName 
                      , importDirectory )
    where

import Message ( Message )

import System.FilePath.Posix
import System.FilePath
import System.Directory
import Data.Char
import Control.Monad
import Data.List

extractName :: FilePath -> Either Message String
extractName fp = case name fp of
                   "" -> Left $ "the file " ++ fp ++ " doesn't contain an account name"
                   n -> Right n

name = filter (not . isDigit) . takeBaseName 

importDirectory :: FilePath -> IO (Either Message [FilePath])
importDirectory fp = do
    contents <- directoryContents fp 
    let filePaths = sort $ map (\fileName -> (fp </> fileName )) contents
    return $ (Right filePaths) >>= checkNotEmpty fp

checkNotEmpty :: FilePath -> [FilePath] ->  Either Message [FilePath]
checkNotEmpty fp [] = Left $ "the directory " ++ fp ++ " doesn't contain import files"
checkNotEmpty _ fs  = Right $ fs

directoryContents :: FilePath -> IO [FilePath]
directoryContents = liftM (filter notDots) . getDirectoryContents
    where 
        notDots p = p /= "." && p /= ".."

            
