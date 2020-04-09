{-# LANGUAGE OverloadedStrings #-}

module Main where

import Report
import Expense
import Category
import CategoriesCsv
import ExpensesCsv
import Command
import Config
import ExitWithMsg
import System.Environment
import System.Directory
import System.Exit
import Control.Monad
import Data.Vector (toList)
import qualified Data.Vector as Vector 
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Text.Printf

help :: IO ()
help = do 
    putStrLn $ unlines ["usage: budget help"
                       ,"       budget summary <BankData.Csv>"
                       ,"       budget summary <BankData.Csv> <CategorySelection.Csv>"]

importCategorySelector :: (Maybe FilePath) -> IO (Either String (Category -> Bool))
importCategorySelector Nothing 
    = return $ pure (const True)
importCategorySelector (Just filePath) = do
    let categories = decodeCategoriesFromFile filePath
    fmap (fmap (flip elem . Vector.toList)) categories

validArgs :: [String] -> Bool
validArgs args = 
    (length args >= 2)
    && args !! 0 == "summary"

main :: IO ()
main = do
    home <- getHomeDirectory
    cfg <- retrieveFromFile $ home ++ "/.budget_conf"
    either exitWithMsg runProgram cfg


runProgram 
    ::  Config 
    -> IO ()
runProgram cfg = do 
    cmd <- fmap command getArgs
    either exitWithMsg (processCommand cfg) cmd

retrieveExpenses 
    :: Config
    -> Maybe FilePath
    -> IO (Either String [Expense])
retrieveExpenses config expenseFilePath = do
    let filePath = case expenseFilePath of
                     Nothing -> lookup "TRANSACTIONS" config 
                     other -> other
    home <- getHomeDirectory
    case filePath of
      Nothing -> do 
          let msg = "error: TRANSACTION file path not found in " ++ home ++ "/.budget_conf"
          return $ Left msg

      Just fp -> do
          let filePath = if (take 2 fp) == ".~" then home ++ (drop 2 fp) else fp
          expenses <- fmap (fmap toList) $ decodeExpensesFromFile filePath
          return expenses


processCommand :: Config -> Command -> IO ()
processCommand _ Help = help
processCommand config (Summary expenseFilePath categoryFilePath) = do

    expenses <- retrieveExpenses config expenseFilePath 
    selector <- importCategorySelector categoryFilePath


    summary expenseFilePath categoryFilePath selector expenses

    exitSuccess
