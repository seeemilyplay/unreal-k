module Main (main) where

import Control.Concurrent.Async
import Data.Functor

import AcidServer
import Config
import DataSource
import InMemoryServer
import WebServer


main :: IO ()
main = do
  c <- configFromArgs
  let parser = if cfguseboxtime c then parsePair else parseTriplet
  if cfgpersist c
    then
      withAcidServer c (runStuff parser)
    else
      withInMemoryServer c (runStuff parser)
  return ()
    where
      runStuff p s = do
        _ <- concurrently (DataSource.listen s (lines <$> getContents) p)
                          (WebServer.start 8000 s)
        return ()
