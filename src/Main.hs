{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)

import qualified Network.MPD as MPD

import qualified Data.Map.Lazy as M

import           Data.Foldable

import Control.Monad.IO.Class

import qualified Data.Text.Lazy as T

import Data.Song

main = scotty 8080 $ do

    middleware $ staticPolicy (noDots >-> addBase "public")
   
    get "/api/playlist" $ do
        response <- liftMPD playlist
        case response of 
            Right songs -> json songs
            
            Left err -> handle err 

--    get "/api/status" $ do 

    get "/api/play" $ do 
        liftMPD $ MPD.play Nothing
        text "Started"

handle err = text . T.pack $ show err

liftMPD = liftIO . MPD.withMPD
