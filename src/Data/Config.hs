{-# LANGUAGE DeriveGeneric #-}
module Data.Config where

import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy as BS

data Config = Config { maxPlaylistLength :: Integer 
                     , refreshTime :: Int
                     , port :: Int
                     } deriving (Show, Generic)

readFromFile :: FilePath -> IO (Either String Config)
readFromFile fp = do 
  file <- BS.readFile fp 
  return $ eitherDecode' file

instance ToJSON Config
instance FromJSON Config
