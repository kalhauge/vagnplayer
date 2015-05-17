{-# LANGUAGE OverloadedStrings #-}

module Data.Song where

import Data.Aeson
import qualified Data.Map as M
import qualified Network.MPD as MPD

data Song = Song { title :: String
                 , artist :: String
                 } deriving (Show)

type Playlist = [Song]


instance ToJSON Song where
    toJSON s = object [ "title" .= title s
                      , "artist" .= artist s] 

toSong :: MPD.Song -> Song
toSong s = Song 
    (getOrDefault s MPD.Title "") 
    (getOrDefault s MPD.Artist "")

toPlaylist :: [MPD.Song] -> Playlist
toPlaylist = map toSong 

playlist :: MPD.MPD Playlist
playlist = do 
    songs <- MPD.playlistInfo Nothing 
    return $ toPlaylist songs
    

getOrDefault :: MPD.Song -> MPD.Metadata -> String -> String
getOrDefault s m def = case M.lookup m tags of
        Just value -> MPD.toString $ head value
        Nothing ->  def
    where tags = MPD.sgTags s
