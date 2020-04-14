module CatchShowIO 
    where

import Control.Exception (IOException)
import qualified Control.Exception as Exception

catchShowIO 
    :: IO a
    -> IO (Either String a)
catchShowIO action =
    fmap Right action `Exception.catch` handleIOException
        where
            handleIOException 
                :: IOException
                -> IO (Either String a)
            handleIOException =
                return . Left . show
