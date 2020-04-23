{-# LANGUAGE OverloadedStrings #-}

module Main where

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

import System.Directory
import System.Environment
import System.Exit
import Control.Monad


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
    let report = (detail filePath ca_filePath category period criteria) <$> categories <*> transactions
    either exitWithMsg (putStr . unlines) report
    exitSuccess

processCommand config (Summary tr_filePath ca_filePath category period criteria) = do
    transactions <- retrieveTransactions config tr_filePath 
    categories     <- maybe (pure (Right [])) decodeCategoriesFromFile ca_filePath
    let report = (summary tr_filePath ca_filePath category period criteria) <$> categories <*> transactions
    either exitWithMsg (putStr . unlines) report
    exitSuccess

processCommand config (Import im_filePath (Just account)) = do
    transactions <- retrieveTransactions config Nothing
    importations <- retrieveTransactions config (Just im_filePath)
    let result_trans = join $ importTransactions account <$> transactions <*> importations 
    let result_length = (-) <$> fmap length result_trans <*> fmap length transactions 
    case result_trans of
      Left msg -> putStrLn msg
      Right tr -> do
            _ <- saveTransactions config tr
            either putStrLn (\n-> putStrLn $ (show n) ++ " transactions imported") result_length

processCommand config (Import im_filePath Nothing) = do
    isDirectory <- doesDirectoryExist im_filePath 
    case isDirectory of
      False -> either exitWithMsg (\name -> processCommand config (Import im_filePath (Just name))) (extractAccountNamePart im_filePath)
      True -> do 

       directory <- importDirectory im_filePath  
       either exitWithMsg (processImportDirectory config) directory

processImportDirectory :: Config.Config -> [FilePath] -> IO ()
processImportDirectory config = mapM_ (\filePath -> processCommand config (Import filePath Nothing))
