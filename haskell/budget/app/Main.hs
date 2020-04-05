{-# LANGUAGE OverloadedStrings #-}

module Main where

import Report
import Bank
import Categories
import System.Environment
import System.Exit
import Control.Monad
import qualified Data.ByteString.Lazy as B (ByteString, readFile)

exitWithMsg msg = do
    putStrLn msg
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    unless (length args >= 1) (exitWithMsg "usage: Report <BankData.Csv>\n       Report <BankData.Csv> <CatagorieSelection.Csv>")
    let fileName = args !! 0
    contents <- B.readFile fileName
    categories <- if length args > 1 then fmap importCategories (readFile (args !! 1)) else return []

    let rep = if null categories then report else reportForCategories categories
    case importFromBank contents of
      Left msg -> exitWithMsg msg
      Right exps -> do
          putStrLn (unlines (rep exps))
          exitSuccess
