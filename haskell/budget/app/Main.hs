{-# LANGUAGE OverloadedStrings #-}

module Main where

import Report
import Expense
import Category
import CategoriesCsv
import System.Environment
import System.Exit
import Control.Monad
import Data.Vector (toList)
import qualified Data.Vector as Vector 
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import Text.Printf

main = undefined
-- exitWithMsg msg = do
--     putStrLn msg
--     exitFailure
-- 
-- importExpenses :: FilePath -> IO (Either String [Expense])
-- importExpenses fileName = do
--     contents <- B.readFile fileName
--     let expenses = importExpensesFromBank contents
--     return expenses
-- 
-- importCategorySelector :: (Maybe FilePath) -> IO (Either String (Category -> Bool))
-- importCategorySelector Nothing 
--     = return $ pure (const True)
-- importCategorySelector (Just filePath) = do
--     let categories = decodeCategoriesFromFile filePath
--     fmap (fmap (flip elem . Vector.toList)) categories
-- 
-- main :: IO ()
-- main = do
--     args <- getArgs
--     unless (length args >= 1) 
--         (exitWithMsg $ unlines ["usage: Report <BankData.Csv>"
--                                ,"       Report <BankData.Csv> <CatagorieSelection.Csv>"])
-- 
--     let fileName = args !! 0
-- 
--     let categoryFilePath = if length args > 1 then Just (args !! 1) else Nothing
-- 
--     importExpenses <- importExpenses fileName
--     selector <- importCategorySelector categoryFilePath
-- 
--     let rep = case categoryFilePath of
--                 Nothing -> reportAllCategories
--                 Just _ -> case selector of
--                             Right f -> reportForCategories f
--                             Left msg -> error $ printf "while importing categories: %s" msg
-- 
--     case importExpenses of
--       Left msg -> exitWithMsg msg
--       Right exps -> do
--           putStrLn (reportTitle fileName categoryFilePath (expensesPeriod exps))
--           putStrLn (unlines (rep exps))
--           exitSuccess
