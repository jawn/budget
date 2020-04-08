{-# LANGUAGE OverloadedStrings #-}

module Main where

import Report
import Expense
import Category
import CategoriesCsv
import ExpensesCsv
import System.Environment
import System.Exit
import Control.Monad
import Data.Vector (toList)
import qualified Data.Vector as Vector 
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Text.Printf

exitWithMsg msg = do
    putStrLn msg
    exitFailure

importCategorySelector :: (Maybe FilePath) -> IO (Either String (Category -> Bool))
importCategorySelector Nothing 
    = return $ pure (const True)
importCategorySelector (Just filePath) = do
    let categories = decodeCategoriesFromFile filePath
    fmap (fmap (flip elem . Vector.toList)) categories

main :: IO ()
main = do
    args <- getArgs
    unless (length args >= 1) 
        (exitWithMsg $ unlines ["usage: Report <BankData.Csv>"
                               ,"       Report <BankData.Csv> <CatagorieSelection.Csv>"])

    let filePath = args !! 0

    let categoryFilePath = if length args > 1 then Just (args !! 1) else Nothing

    expenses <- fmap (fmap toList) $ decodeExpensesFromFile filePath
    selector <- importCategorySelector categoryFilePath

    let rep = case categoryFilePath of
                Nothing -> reportAllCategories
                Just _ -> case selector of
                            Right f -> reportForCategories f
                            Left msg -> error $ printf "while importing categories: %s" msg

    case expenses of
      Left msg -> exitWithMsg msg
      Right exps -> do
          putStrLn (reportTitle filePath categoryFilePath (expensesPeriod exps))
          putStrLn (unlines (rep exps))
          exitSuccess
