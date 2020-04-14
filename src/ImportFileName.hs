module ImportFileName 
    where

import System.FilePath.Posix
import System.FilePath
import System.Directory
import Data.Char
import Control.Monad
import Data.List

extractName :: FilePath -> Either String String
extractName fp = case name fp of
                   "" -> Left $ "the file " ++ fp ++ " doesn't contain an account name"
                   n -> Right n

name = filter (not . isDigit) . takeBaseName 

importDirectory :: FilePath -> IO (Either String [FilePath])
importDirectory fp = do
    contents <- directoryContents fp 
    let filePaths = sort $ map (\fileName -> (fp </> fileName )) contents
    return $ (Right filePaths) >>= checkNotEmpty fp

checkNotEmpty :: FilePath -> [FilePath] ->  Either String [FilePath]
checkNotEmpty fp [] = Left $ "the directory " ++ fp ++ " doesn't contain import files"
checkNotEmpty _ fs  = Right $ fs

directoryContents :: FilePath -> IO [FilePath]
directoryContents = liftM (filter notDots) . getDirectoryContents
    where 
        notDots p = p /= "." && p /= ".."

            
