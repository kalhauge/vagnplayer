{-# LANGUAGE OverloadedStrings #-}

module Data.Song where

import Data.Aeson

data Song = Song { title :: String
                 , artist :: String
                 } deriving (Show)

type Playlist = [Song]

instance ToJSON Song where
    toJSON s = object [ "title" .= title s
                      , "artist" .= artist s] 
