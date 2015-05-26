{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Network.Wai.Middleware.Static as S
import           Web.Scotty

import qualified Network.MPD                   as MPD

import           Control.Monad.IO.Class
-- import           Control.Monad

import qualified Data.Text.Lazy                as T

import           Data.Song
import           Data.Status
import           Data.Control

main :: IO ()
main = scotty 8080 $ do

  middleware $ S.staticPolicy (S.noDots >-> S.addBase "public")

  get "/" $ file "public/index.html"

  get "/api/playlist" $ do
    songs <- liftMPD playlist
    json songs

  get "/api/status" $ do
    status <- liftMPD vagnstatus
    json status
  
  put "/api/control" $ do
    (x :: Control) <- jsonData
    liftMPD $ control x
    text "OK";


liftMPD :: MPD.MPD a -> ActionM a
liftMPD x = do 
    response <- liftIO . MPD.withMPD $ x
    case response of
      Right value -> return value
      Left err -> do 
        handle err 
        return undefined

handle :: Show a => a -> ActionM ()
handle err = raise . T.pack $ show err

