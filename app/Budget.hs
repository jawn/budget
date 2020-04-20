{-# LANGUAGE OverloadedStrings #-}

module Main where

import Summary
import Detail
import Transaction
import Category
import CategoriesCsv
import TransactionsCsv
import Command
import Sorting
import Import
import ImportFileName
import qualified Config as Config
import ExitWithMsg
import Help
import System.Environment
import System.Directory
import System.Exit
import Control.Monad
import Data.Vector (toList)
import qualified Data.Vector as Vector 
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Text.Printf


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

processCommand config (Detail filePath categoriesFilePath category period criteria) = do 
    transactions <- retrieveTransactions config filePath 
    printDetail filePath categoriesFilePath category period criteria transactions 
    exitSuccess

processCommand config (Summary tr_filePath ca_filePath period criteria) = do
    transactions <- retrieveTransactions config tr_filePath 
    selector     <- importCategorySelector ca_filePath
    printSummary tr_filePath ca_filePath period criteria selector transactions 
    exitSuccess

processCommand config (Import im_filePath (Just account)) = do
    transactions <- retrieveTransactions config Nothing
    importations <- retrieveTransactions config (Just im_filePath)
    let result_trans = join $ importTransactions account <$> transactions <*> importations 
    let result_length = (-) <$> fmap length result_trans <*> fmap length transactions 
    case result_trans of
      Left msg -> putStrLn msg
      Right tr -> do
            saveTransactions config tr
            either putStrLn (\n-> putStrLn $ (show n) ++ " transactions imported") result_length

processCommand config (Import im_filePath Nothing) = do
    isDirectory <- doesDirectoryExist im_filePath 
    case isDirectory of
      False -> either exitWithMsg (\name -> processCommand config (Import im_filePath (Just name))) (extractName im_filePath)
      True -> do 

       directory <- importDirectory im_filePath  
       either exitWithMsg (processImportDirectory config) directory

processImportDirectory :: Config.Config -> [FilePath] -> IO ()
processImportDirectory config = mapM_ (\filePath -> processCommand config (Import filePath Nothing))
