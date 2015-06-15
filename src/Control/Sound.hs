{-# LANGUAGE OverloadedStrings #-}
module Control.Sound where

import Network.MPD

import Data.Maybe
import Data.Ix (inRange)

--import Data.Random.Extras
import Data.Random
import Data.Foldable

import Control.Monad.IO.Class (liftIO)

addRandomSong :: MPD Song
addRandomSong = do 
  songs <- foldl' onlySongs [] <$> listAllInfo "Local media" 
  song <- liftIO $ runRVar (randomElement songs) StdRandom
  add (sgFilePath song)
  return song

  where
    onlySongs :: [Song] -> LsResult -> [Song]
    onlySongs ss (LsSong song) = ss ++ [song]
    onlySongs ss _ = ss

-- Crops the active playlist to only contain size items
cropPlaylist :: Int -> MPD ()
cropPlaylist size = do
  i <- fromMaybe 0 <$> stSongPos <$> status
  crop (i, i + size - 1)

-- Crops the active playlist only leaving items withing range
crop :: (Position, Position) -> MPD ()
crop range = do 
  songs <- playlistInfo Nothing 
  
  mapM_ deleteId $ mapMaybe sgId $ filter songsNotInRange songs 

  where songsNotInRange :: Song -> Bool
        songsNotInRange s = 
          maybe False (not . inRange range) $ sgIndex s



  

