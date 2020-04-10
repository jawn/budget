{-# LANGUAGE OverloadedStrings #-}

module Main where

import Summary
import Transaction
import Category
import CategoriesCsv
import TransactionsCsv
import Command
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


processCommand :: Config.Config -> Command -> IO ()
processCommand _ Help = help
processCommand config (Summary transactionFilePath categoryFilePath) = do

    transactions <- retrieveTransactions config transactionFilePath 
    selector <- importCategorySelector categoryFilePath


    printSummary transactionFilePath categoryFilePath selector transactions

    exitSuccess
