module Domain ( Domain
              , catchIODomain
              )
    where

import Message

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad.Except

type Domain = ExceptT Message IO 

catchIODomain :: IO a -> Domain a
catchIODomain action = ExceptT (catchIO action)
    where
    catchIO :: IO a -> IO (Either Message a)
    catchIO a = 
        fmap Right a `Exception.catch` handleIOException
    
    handleIOException :: IOException -> IO (Either Message a)
    handleIOException = return . Left . show
