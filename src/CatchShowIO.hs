module CatchShowIO ( catchShowIO, catchShowIOE )
    where

import Message

import Control.Exception (IOException)
import Control.Monad.Except
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

catchShowIOE 
    :: IO a
    -> ExceptT Message IO a
catchShowIOE action = ExceptT (catchShowIO action) 
