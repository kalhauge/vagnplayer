{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           System.Environment (getArgs)

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
import           Control.Sound

import           Data.Config
import           Data.Song
import           Data.Status
import           Data.Control
import           Data.Functor

main :: IO ()
main = do 
  args <- getArgs

  some <- readFromFile (head args) 
  config <- case some of
    Right config -> return config
    Left string -> fail string

  MPD.withMPD $ do
    MPD.clear
    MPD.add "local:track:startup.mp3"  
    MPD.play Nothing 
  
  every (refreshTime config * 1000) $ setupMPD config
  
  scotty (port config) $ do

    middleware $ S.staticPolicy (S.noDots >-> S.addBase "public")
    middleware logStdoutDev

    get "/" $ file "public/index.html"

    get "/api/playlist" $ 
      liftMPD playlist >>= handle >>= json
    
    put "/api/playlist" $ do
      song <- jsonData
      
      let mpl = maxPlaylistLength config

      handle =<< liftMPD (do 
        status <- MPD.status
        if MPD.stPlaylistLength status >= mpl then
          fail "Could not add song"
        else addSong song)
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

setupMPD :: Config -> IO ()
setupMPD config = do 
  x <- MPD.withMPD $ do 
    
    -- Always consume
    MPD.consume True

    -- Get status 
    status <- MPD.status

    liftIO $ print (MPD.stTime status) 

    -- If almost empty, refill
    when (almostEmpty (refreshTime config - 5) status) $ do
      song <- addRandomSong 
      liftIO $ do 
        putStr "Added "
        print song
  
    let mpl = maxPlaylistLength config
    -- If too long crop it
    when (MPD.stPlaylistLength status > mpl) $ 
      cropPlaylist (fromIntegral mpl)

    -- If not playling, start playing
    case MPD.stState status of
      MPD.Playing -> return ()
      otherwise -> void $ MPD.play Nothing

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

