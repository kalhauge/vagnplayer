{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Song where

import Data.Aeson
import Data.String
import GHC.Generics

import qualified Data.Map as M
import qualified Network.MPD as MPD

import qualified Data.Text                as T

data Song = Song { title :: String
                 , artist :: String
                 , length :: Integer
                 , path :: Path
                 , index :: Maybe Int 
                 } deriving (Show, Generic)

instance ToJSON Song

toSong :: MPD.Song -> Song
toSong s = Song 
    (getOrDefault s MPD.Title "") 
    (getOrDefault s MPD.Artist "")
    (MPD.sgLength s)
    (Path $ MPD.sgFilePath s)
    (MPD.sgIndex s)

newtype Path = Path MPD.Path deriving (Show)

instance ToJSON Path where
    toJSON (Path path) = String $ T.pack $ MPD.toString path 

toPath :: T.Text -> MPD.Path
toPath text = fromString $ T.unpack text


fromPath :: Path -> MPD.Path
fromPath (Path path) = path

type Playlist = [Song]

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
