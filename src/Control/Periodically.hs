module Control.Periodically where

import Control.Concurrent
import Control.Monad

every :: Int -> IO () -> IO ()
every millis f = void $ forkIO (forever (f >> threadDelay (millis * 1000)))

