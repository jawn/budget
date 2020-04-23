module CatchShowIO ( catchShowIO )
    where

import Message

import Control.Exception (IOException)
import qualified Control.Exception as Exception

catchShowIO 
    :: IO a
    -> IO (Either Message a)
catchShowIO action =
    fmap Right action `Exception.catch` handleIOException
        where
            handleIOException 
                :: IOException
                -> IO (Either Message a)
            handleIOException = return . Left . show
