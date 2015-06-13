{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Network.Wai.Middleware.Static as S
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

import           Network.MPD (MPD)
import qualified Network.MPD                   as MPD

import           Control.Monad
import           Control.Monad.IO.Class
-- import           Control.Monad
import           Data.String

import qualified Data.Text.Lazy                as T

import           Control.Periodically

import           Data.Song
import           Data.Status
import           Data.Control
import           Data.Functor


main :: IO ()
main = do 
  every 60000 setupMPD
  scotty 8080 $ do

    middleware $ S.staticPolicy (S.noDots >-> S.addBase "public")
    middleware logStdoutDev

    get "/" $ file "public/index.html"

    get "/api/playlist" $ 
      liftMPD playlist >>= handle >>= json
    
    put "/api/playlist" $ do
      song <- jsonData
      liftIO $ print song
      handle =<< liftMPD (addSong song)
      text "OK"

    get "/api/status" $ 
      liftMPD vagnstatus >>= handle >>= json

    get "/api/song" $ do
      (query, rest) <- parseParams <$> params
     
      songs <- toPlaylist <$> (handle =<< liftMPD (MPD.search query))
      
      let limit = case lookup "limit" rest of  
              Just x -> case parseParam x of
                  Right x -> x
                  Left msg -> 10
              Nothing -> 10
      json $ take limit songs
       
    
    put "/api/control" $ do
      (x :: Control) <- jsonData
      liftMPD $ control x
      text "OK"

setupMPD :: IO ()
setupMPD = do 
  x <- MPD.withMPD $ do 
    -- Always consume
    MPD.consume True

    -- Get status 
    status <- MPD.status
    -- If almost empty, refill
    if MPD.stPlaylistLength status <= 2 then
      -- Grap random local song 
      liftIO $ print "bad"
    else 
      liftIO $ print "good"

    playlists <- MPD.listPlaylists

    liftIO $ print playlists

    all <- MPD.config

    liftIO $ print all

  case x of 
    Right x -> return ()
    Left err -> print err



liftMPD :: MPD a -> ActionM (MPD.Response a)
liftMPD x = liftIO . MPD.withMPD $ x

raiseShowable :: Show a => a -> ActionM b
raiseShowable err = raise . T.pack $ show err

handle :: MPD.Response a -> ActionM a
handle response =
  case response of
    Right value -> return value
    Left err -> raiseShowable err 

parseParams :: [Param] -> (MPD.Query, [Param])
parseParams = foldl parseQueryParam (MPD.anything, [])

parseQueryParam :: (MPD.Query, [Param]) -> Param -> (MPD.Query, [Param])
parseQueryParam (a, ps) ("title", title) = 
    (a MPD.<&> MPD.Title MPD.=? toValue title, ps) 
parseQueryParam (a, ps) ("artist", artist) = 
    (a MPD.<&> MPD.Artist MPD.=? toValue artist, ps)
parseQueryParam (a, ps) p = (a, p:ps)

toValue :: T.Text -> MPD.Value
toValue = fromString . T.unpack

