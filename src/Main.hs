{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Network.Wai.Middleware.Static as S
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

import           Network.MPD (MPD, (<&>), (=?))
import qualified Network.MPD                   as MPD

import           Control.Monad.IO.Class
-- import           Control.Monad
import           Data.String

import qualified Data.Text.Lazy                as T

import           Data.Song
import           Data.Status
import           Data.Control


main :: IO ()
main = scotty 8080 $ do

  middleware $ S.staticPolicy (S.noDots >-> S.addBase "public")
  middleware logStdoutDev

  get "/" $ file "public/index.html"

  get "/api/playlist" $ do
    songs <- liftMPD playlist
    json songs

  get "/api/status" $ do
    status <- liftMPD vagnstatus
    json status

  get "/api/song" $ do
    (query, rest) <- parseParams <$> params
    
    songs <- toPlaylist <$> liftMPD (MPD.search query) 
   
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

liftMPD :: MPD a -> ActionM a
liftMPD x = do 
    response <- liftIO . MPD.withMPD $ x
    case response of
      Right value -> return value
      Left err -> do 
        handle err 
        return undefined

handle :: Show a => a -> ActionM ()
handle err = raise . T.pack $ show err

parseParams :: [Param] -> (MPD.Query, [Param])
parseParams = foldl parseQueryParam (MPD.anything, [])

parseQueryParam :: (MPD.Query, [Param]) -> Param -> (MPD.Query, [Param])
parseQueryParam (a, ps) ("title", title) = 
    (a <&> MPD.Title =? toValue title, ps) 
parseQueryParam (a, ps) ("artist", artist) = 
    (a <&> MPD.Artist =? toValue artist, ps)
parseQueryParam (a, ps) p = (a, p:ps)

toValue :: T.Text -> MPD.Value
toValue = fromString . T.unpack

