module VersionNumber 
    where

data VersionNumber = VersionNumber { major :: Int
                                   , minor :: Int
                                   , patch :: Int
                                   }

instance Show VersionNumber 
    where
        show v = "v" ++ show (major v) ++ "."
                     ++ show (minor v) ++ "."
                     ++ show (patch v)


versionNumber :: VersionNumber
versionNumber = VersionNumber { major = 1
                              , minor = 0
                              , patch = 0
                              }
