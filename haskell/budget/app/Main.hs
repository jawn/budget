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

importCategorySelector :: (Maybe FileName) -> IO (Either String (Category -> Bool))
importCategorySelector Nothing = return $ return (const True)
importCategorySelector (Just fileName) = do
    contents <- readFile fileName
    let categories = importCategoriesFromList contents
    return $ fmap (\cats -> (`elem` cats)) categories

main :: IO ()
main = do
    args <- getArgs
    unless (length args >= 1) 
        (exitWithMsg $ unlines ["usage: Report <BankData.Csv>"
                               ,"       Report <BankData.Csv> <CatagorieSelection.Csv>"])

    let fileName = args !! 0

    let categoryFileName = if length args > 1 then Just (args !! 1) else Nothing

    importExpenses <- importExpenses fileName
    selector <- importCategorySelector categoryFileName

    let rep = case categoryFileName of
                Nothing -> reportAllCategories
                Just _ -> case selector of
                            Right f -> reportForCategories f
                            Left msg -> error $ printf "while importing categories: %s" msg

    case importExpenses of
      Left msg -> exitWithMsg msg
      Right exps -> do
          putStrLn (reportTitle fileName categoryFileName (expensesPeriod exps))
          putStrLn (unlines (rep exps))
          exitSuccess
