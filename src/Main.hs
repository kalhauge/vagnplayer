{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)

import qualified Network.MPD as MPD

import qualified Data.Map.Lazy as M

import           Data.Foldable

import Control.Monad.IO.Class

import Data.Song

main = scotty 8080 $ do

    middleware $ staticPolicy (noDots >-> addBase "public")
   
    get "/api/playlist" $ do
        list <- liftIO playlist
        json list

--    get "/api/status" $ do 

    get "/api/play" $ do 
        liftIO $ MPD.withMPD $ MPD.play Nothing
        text "Started"

playlist :: IO Playlist
playlist = do 
        Right songs <- MPD.withMPD $ MPD.playlistInfo Nothing
        return $ map toSong songs
    where
        toSong s = Song 
            (getOrDefault s MPD.Title "") 
            (getOrDefault s MPD.Artist "")

getOrDefault :: MPD.Song -> MPD.Metadata -> String -> String
getOrDefault s m def = case M.lookup m tags of
        Just value -> MPD.toString $ head value
        Nothing ->  def
    where tags = MPD.sgTags s


sndMain :: IO ()
sndMain = do 
    putStrLn "VAGON IN DA HOUSE"
    -- MPD.withMPD $ MPD.play Nothing
    response <- MPD.withMPD $ MPD.playlistInfo Nothing
    case response of 
       (Right songs) -> forM_ songs $ \song -> do
           let x = MPD.sgTags song
           let (Just title) = M.lookup MPD.Title x
           print title


