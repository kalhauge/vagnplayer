{-# LANGUAGE OverloadedStrings #-}
module Data.Control where

import Data.Aeson

import qualified Network.MPD as MPD

data Command = Play
             | Pause
         --    | Stop
             | Prev
             | Next
             deriving (Show, Read)

instance ToJSON Command where
  toJSON Play = "PLAY"
  toJSON Pause = "PAUSE"
  toJSON Prev = "PREV"
  toJSON Next = "NEXT"
  -- toJSON Stop = "STOP"

instance FromJSON Command where
  parseJSON (String s) = return $ case s of
    "PLAY" -> Play
    "PAUSE" -> Pause
    "PREV" -> Prev
    "NEXT" -> Next
    -- _ -> fail $ "Could not parse" ++ s

data Control = Control { cmd :: Command } deriving (Show)

instance FromJSON Control where
  parseJSON (Object v) = Control <$> v .: "cmd"

control :: Control -> MPD.MPD ()
control (Control Play) = MPD.play Nothing
control (Control Pause) = MPD.pause True
control (Control Next) = MPD.next
control (Control Prev) = MPD.previous
