{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Message
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
import Transaction

import Control.Monad.Except
import System.Directory
import System.Environment
import System.Exit

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
    _ <- liftIO $ processCommand cfg cmd
    liftIO $ putStrLn $ show cmd
    

runProgram ::  Configuration -> IO ()
runProgram cfg = do
    cmd <- fmap command getArgs
    either exitWithMsg (processCommand cfg) cmd


processCommand
    :: Configuration
    -> Command
    -> IO ()

processCommand _ (Help arg) = help arg

processCommand config (Detail filePath ca_filePath category period criteria) = do
    transactions <- retrieveTransactions config filePath
    categories     <- maybe (pure (Right [])) decodeCategoriesFromFile ca_filePath
    let report = detail filePath ca_filePath category period criteria <$> categories <*> transactions
    either exitWithMsg (putStr . unlines) report
    exitSuccess

processCommand config (Summary tr_filePath ca_filePath category period criteria) = do
    transactions <- retrieveTransactions config tr_filePath
    categories     <- maybe (pure (Right [])) decodeCategoriesFromFile ca_filePath
    let report = (summary tr_filePath ca_filePath category period criteria) <$> categories <*> transactions
    either exitWithMsg (putStr . unlines) report
    exitSuccess

processCommand config (Import im_filePath (Just account)) =
  either putStrLn return =<< runExceptT (processImport config im_filePath account)

processCommand config (Import im_filePath Nothing) = do
    isDirectory <- doesDirectoryExist im_filePath
    case isDirectory of
      False -> either exitWithMsg (\name -> processCommand config (Import im_filePath (Just name))) (extractAccountNamePart im_filePath)
      True -> do

       directory <- importDirectory im_filePath
       either exitWithMsg (processImportDirectory config) directory

processImportDirectory :: Configuration -> [FilePath] -> IO ()
processImportDirectory config = mapM_ (\filePath -> processCommand config (Import filePath Nothing))


-- MTL Style
class (Monad m) => Transactional m where
    retrieveTransactionsT :: Configuration -> Maybe FilePath -> m [Transaction]
    saveTransactionsT :: Configuration -> [Transaction] -> m ()
    importTransactionsT :: String -> [Transaction] -> [Transaction] -> m ([Transaction],[Transaction])
    reportT :: String -> m ()

instance Transactional (ExceptT Message IO) where
    retrieveTransactionsT = retrieveTransactionsE
    saveTransactionsT = saveTransactionsE
    importTransactionsT acc txs imps = ExceptT $ return $ importTransactions acc txs imps
    reportT = liftIO . putStrLn

-- processImport :: Configuration
--               -> FilePath -> String -> ExceptT Message IO ()
processImport :: Transactional m =>
                 Configuration -> FilePath -> String -> m ()
processImport config im_filePath account = do
    transactions <- retrieveTransactionsT config Nothing
    importations <- retrieveTransactionsT config (Just im_filePath)
    result <- importTransactionsT account transactions importations
    let result_length = length (fst result) - length transactions
    let result_dupes  = snd result
    let result_new_trans = fst result
    saveTransactionsT config result_new_trans
    reportT $ show result_length ++ " transactions imported"
    reportT $ case result_dupes of
                [] -> "no duplicates were found"
                _ -> unlines ([ "transactions that where already in the main file and were not imported:" ] 
                              ++ (showDuplicates result_dupes))
