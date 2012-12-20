module Main (main) where

import Control.Concurrent.Async
import Data.Functor

import AcidServer
import DataSource
import WebServer


main :: IO ()
main = withAcidServer True 100 (\t -> t - 86400) $ \acid -> do
  _ <- concurrently (DataSource.listen acid (lines <$> getContents) parseTriplet)
                    (WebServer.start 8000 acid)
  return ()
