{-# LANGUAGE OverloadedStrings #-}

module Main where

import Report
import Expense
import Bank
import Categories
import System.Environment
import System.Exit
import Control.Monad
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Text.Printf


exitWithMsg msg = do
    putStrLn msg
    exitFailure

importExpenses :: FileName -> IO (Either String [Expense])
importExpenses fileName = do
    contents <- B.readFile fileName
    let expenses = importExpensesFromBank contents
    return expenses

importCategorySelector :: (Maybe FileName) -> IO (Category -> Bool)
importCategorySelector Nothing = return (const True)
importCategorySelector (Just fileName) = do
    contents <- readFile fileName
    let categories = importCategoriesFromList contents
    return (`elem` categories)

main :: IO ()
main = do
    args <- getArgs
    unless (length args >= 1) 
        (exitWithMsg "usage: Report <BankData.Csv>\n       Report <BankData.Csv> <CatagorieSelection.Csv>")

    let fileName = args !! 0

    let categoryFileName = if length args > 1 then Just (args !! 1) else Nothing

    expenses <- importExpenses fileName
    selector <- importCategorySelector categoryFileName

    let rep = case categoryFileName of
                Nothing -> reportAllCategories
                Just _ -> reportForCategories selector

    case expenses of
      Left msg -> exitWithMsg msg
      Right exps -> do
          putStrLn (reportTitle fileName categoryFileName (period exps))
          putStrLn (unlines (rep exps))
          exitSuccess
