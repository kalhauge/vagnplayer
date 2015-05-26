{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Status where

import Safe
import Data.Song 

import Data.Aeson
import Debug.Trace

import qualified Network.MPD as MPD

import GHC.Generics

newtype State = State MPD.State deriving (Show)

instance ToJSON State where
    toJSON (State s) = case s of
        MPD.Playing -> "PLAYING"
        MPD.Stopped -> "STOPPED"
        MPD.Paused -> "PAUSED"

data Status = Status { song :: Maybe Song 
                     , time :: Maybe Double
                     , state :: State
                     } deriving (Show, Generic)

instance ToJSON Status

vagnstatus :: MPD.MPD Status
vagnstatus = do
    s <- MPD.status
    currentSongs <- MPD.playlistInfo $ MPD.stSongPos s
    return Status { song = toSong <$> headMay currentSongs
                  , time = seconds <$> MPD.stTime s
                  , state = State $ MPD.stState s
                  }

seconds :: (Double, MPD.Seconds) -> Double
seconds (m, _) = m

