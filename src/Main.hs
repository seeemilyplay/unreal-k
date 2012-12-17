module Main (main) where

import Control.Concurrent.Async
import Data.Functor

import AcidServer
import DataSource
import WebServer


main :: IO ()
main = withAcidServer 100 (\t -> t - 36000) $ \acid -> do
  _ <- concurrently (DataSource.listen acid (lines <$> getContents) parsePair)
                    (WebServer.start 8000 acid)
  return ()
