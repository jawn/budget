
module Main where

import Category
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
    cmd <- ExceptT ((return . command) args)
    doCommand cfg cmd

doCommand :: Configuration -> Command -> Domain ()
doCommand configuration (Detail maybeFilePath maybeCatFilePath maybeCategory maybePeriod criteria) = do
    mainFilePath <- configuration `atKey` "TRANSACTIONS"   
    transactions <- transactionsFromFile (maybe mainFilePath id maybeFilePath)
    categories <- liftIO (maybe (pure (Right [])) decodeCategoriesFromFile maybeCatFilePath)
    let report = detail maybeFilePath maybeCatFilePath maybeCategory maybePeriod criteria <$> categories <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand configuration (Summary maybeFilePath maybeCatFilePath maybeCategory maybePeriod criteria) = do 
    mainFilePath <- configuration `atKey` "TRANSACTIONS"
    transactions <- transactionsFromFile (maybe mainFilePath id maybeFilePath)
    categories <- liftIO (maybe (pure (Right [])) decodeCategoriesFromFile maybeCatFilePath)
    let report = (summary maybeFilePath maybeCatFilePath maybeCategory maybePeriod criteria) <$> categories <*> pure transactions
    liftIO (either exitWithMsg (putStr . unlines) report)

doCommand configuration (Import importFilePath (Just account)) = do
    mainFilePath <- configuration `atKey` "TRANSACTIONS"
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

doCommand configuration (Import importFilePath Nothing) = do
    isDirectory <- liftIO (doesDirectoryExist importFilePath)
    case isDirectory of
        False -> do
              let name = extractAccountNamePart importFilePath
              either (liftIO . exitWithMsg) (\n -> doCommand configuration (Import importFilePath (Just n))) name
        True -> do
            directory <- liftIO (importDirectory importFilePath)
            either (liftIO . exitWithMsg) (mapM_ (\filePath -> doCommand configuration (Import filePath Nothing))) directory
            

        
doCommand _ (Help arg) = liftIO (help arg)

