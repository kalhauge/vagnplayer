{-# LANGUAGE OverloadedStrings #-}
module Control.Sound where

import Network.MPD

import Data.Maybe
import Data.Ix (inRange)

import System.Random

--import Data.Random.Extras
--import Data.Random
import Data.Foldable

import Control.Monad.IO.Class (liftIO)

randomElement :: [a] -> IO a
randomElement as = do
    index <- getStdRandom $ randomR (0, size -1)
    return $ as !! index
  where size = length as

addRandomSong :: MPD Song
addRandomSong = do 
  songs <- foldl' onlySongs [] <$> listAllInfo "Local media" 
  song <- liftIO $ randomElement songs
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



  

