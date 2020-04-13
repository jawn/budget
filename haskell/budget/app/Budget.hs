{-# LANGUAGE OverloadedStrings #-}

module Main where

import Summary
import Detail
import Transaction
import Category
import CategoriesCsv
import TransactionsCsv
import Command
import Import
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

processCommand _ Help = help

processCommand config (Detail filePath category period criteria) = do 
    transactions <- retrieveTransactions config filePath 
    printDetail filePath category period criteria transactions 
    exitSuccess

processCommand config (Summary tr_filePath ca_filePath) = do
    transactions <- retrieveTransactions config tr_filePath 
    selector     <- importCategorySelector ca_filePath
    printSummary tr_filePath ca_filePath selector transactions
    exitSuccess

processCommand config (Import im_filePath account) = do
    transactions <- retrieveTransactions config Nothing
    importations <- retrieveTransactions config (Just im_filePath)
    case transactions of
        Left msg -> exitWithMsg msg
        Right ts -> case importations of
                      Right imp -> do
                          result <- importTransactionFile config account ts imp 
                          case result of 
                            Right n -> do putStrLn $ show n ++ " transactions imported"
                                          exitSuccess
                            Left msg -> exitWithMsg msg
                      Left msg -> exitWithMsg msg

