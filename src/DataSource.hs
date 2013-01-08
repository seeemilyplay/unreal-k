module DataSource (listen, parsePair, parseTriplet) where

import Data.Functor
import Data.UnixTime

import Storage


listen :: (Storage s) => s -> IO [a] -> (Time -> a -> [(Key, Item)]) -> IO ()
listen s source parse = do
  xs <- source
  mapM_ process xs
  return ()
  where
    process x = do
      t <- fromEnum . utSeconds <$> getUnixTime
      mapM (\(k,i) -> save s k i) $ parse t x

parsePair :: Time -> String -> [(Key, Item)]
parsePair t xs =
  case words xs of
    [] -> []
    (k:e:[]) -> [(k,(e, 1, t))]
    _ -> []

parseTriplet :: Time -> String -> [(Key, Item)]
parseTriplet _ xs =
  case words xs of
    [] -> []
    --todo: and the div 10 bit
    (k:e:t:[]) -> [(k,(e, 1, fromIntegral $ (read t :: Integer)))]
    _ -> []
