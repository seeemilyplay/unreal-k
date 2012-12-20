module Main (main) where

import Control.Concurrent.Async
import Data.Functor

--import AcidServer
import DataSource
import InMemoryServer
import WebServer


main :: IO ()
main = withInMemoryServer 100 (\t -> t - 86400) $ \s -> do
  _ <- concurrently (DataSource.listen s (lines <$> getContents) parseTriplet)
                    (WebServer.start 8000 s)
  return ()
