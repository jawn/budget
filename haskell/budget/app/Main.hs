module Main where

import Report
import Bank
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
    unless (length args == 1) (exitWithMsg "usage: Report <BankData.Csv>")
    let fileName = args !! 0
    contents <- B.readFile fileName
    case importFromBank contents of
      Left msg -> exitWithMsg msg
      Right exps -> do
          putStrLn (unlines (report exps))
          exitSuccess
