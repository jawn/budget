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
catchIODomain action = ExceptT (action `catch` handle)
    where
    catch :: IO a -> (IOException -> IO (Either Message a)) -> IO (Either Message a)
    a `catch` h = fmap Right a `Exception.catch` h

    handle :: IOException -> IO (Either Message a)
    handle = return . Left . show
