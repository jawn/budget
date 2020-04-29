
module Main where

import CategoryList
import Command
import Configuration
import Detail
import Domain
import ExitWithMsg
import Help
import Import
import ImportFileName
import Summary
import TransactionList

import Control.Monad.Except
import System.Directory
import System.Environment

main :: IO ()
main = do
    run <- runExceptT budget
    either putStrLn return run

configFilePath :: FilePath
configFilePath = "/.budget_conf"

budget :: Domain ()
budget = do
    args <- liftIO getArgs
    home <- liftIO getHomeDirectory
    cfg <- fromFile (home ++ configFilePath)
    cmd <- command args
    doCommand cfg cmd

doCommand :: Configuration -> Command -> Domain ()
doCommand cfg (Detail mFilePath mCatFilePath mCategory mPeriod criteria) = do
    mainFilePath <- cfg `atKey` "TRANSACTIONS"   
    transactions <- transactionsFromFile (maybe mainFilePath id mFilePath)
    categories   <- maybe (domain (Right [])) categoriesFromFile mCatFilePath
    let report = detail mFilePath mCatFilePath mCategory mPeriod criteria <$> pure categories <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand cfg (Summary mFilePath mCatFilePath mCategory mPeriod criteria) = do 
    mainFilePath <- cfg `atKey` "TRANSACTIONS"
    transactions <- transactionsFromFile (maybe mainFilePath id mFilePath)
    categories   <- maybe (domain (Right [])) categoriesFromFile mCatFilePath
    let report = (summary mFilePath mCatFilePath mCategory mPeriod criteria) <$> pure categories <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand cfg (Import importFilePath (Just account)) = do
    mainFilePath <- cfg `atKey` "TRANSACTIONS"
    transactions <- transactionsFromFile mainFilePath
    importations <- transactionsFromFile importFilePath
    result <- domain (importTransactions account transactions importations)
    let result_length = length (fst result) - length transactions
    let result_dupes  = snd result
    let result_new_trans = fst result
    transactionsToFile mainFilePath result_new_trans
    liftIO (putStrLn (show result_length ++ " transactions imported"))
    liftIO (putStrLn (case result_dupes of
                        [] -> "no duplicates were found"
                        _ -> unlines ([ "transactions that were already in the main file and were not imported:" ] 
                              ++ (showDuplicates result_dupes))))

doCommand cfg (Import importFilePath Nothing) = do
    isDirectory <- liftIO (doesDirectoryExist importFilePath)
    case isDirectory of
        False -> do
              let name = extractAccountNamePart importFilePath
              either (liftIO . exitWithMsg) (\n -> doCommand cfg (Import importFilePath (Just n))) name
        True -> do
            directory <- liftIO (importDirectory importFilePath)
            either (liftIO . exitWithMsg) (mapM_ (\filePath -> doCommand cfg (Import filePath Nothing))) directory
            

        
doCommand _ (Help arg) = liftIO (help arg)

