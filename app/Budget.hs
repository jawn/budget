{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Message
import Category
import Command
import Config
import Detail
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
    home <- getHomeDirectory
    cfg <- Config.fromFile $ home ++ "/.budget_conf"
    either exitWithMsg runProgram cfg


runProgram ::  Config.Config -> IO ()
runProgram cfg = do
    cmd <- fmap command getArgs
    either exitWithMsg (processCommand cfg) cmd


processCommand
    :: Config.Config
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

processImportDirectory :: Config.Config -> [FilePath] -> IO ()
processImportDirectory config = mapM_ (\filePath -> processCommand config (Import filePath Nothing))


-- MTL Style
class (Monad m) => Transactional m where
  retrieveTransactionsT :: Config -> Maybe FilePath -> m [Transaction]
  saveTransactionsT :: Config -> [Transaction] -> m ()
  importTransactionsT :: String -> [Transaction] -> [Transaction] -> m ([Transaction],[Transaction])
  reportT :: String -> m ()

-- record of functions
data TransactionalR m =
  TransactionalR { retrieveTransactionsR :: Config -> Maybe FilePath -> m [Transaction] }

-- free-monad style
data TransactionalF a where
  RetrieveTransaction :: Config -> Maybe FilePath -> TransactionalF [Transaction]

instance Transactional (ExceptT Message IO) where
  retrieveTransactionsT = retrieveTransactionsE
  saveTransactionsT = saveTransactionsE
  importTransactionsT acc txs imps = ExceptT $ return $ importTransactionsDelta acc txs imps
  reportT = liftIO . putStrLn

-- processImport :: Config
--               -> FilePath -> String -> ExceptT Message IO ()
processImport :: Transactional m =>
                 Config -> FilePath -> String -> m ()
processImport config im_filePath account = do
    transactions <- retrieveTransactionsT config Nothing
    importations <- retrieveTransactionsT config (Just im_filePath)
    result <- importTransactionsT account transactions importations
    let result_length = length (fst result) - length transactions
    let result_dupes  = snd result
    let result_new_trans = fst result
    saveTransactionsT config result_new_trans
    reportT $ show result_length ++ " transactions imported"
    reportT $ show result_dupes
