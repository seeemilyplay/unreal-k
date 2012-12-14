module DataSource (listen, parsePair) where

import Data.Functor
import Data.UnixTime

import AcidServer


listen :: AcidServer -> IO [a] -> (Time -> a -> [(Key, Item)]) -> IO ()
listen acid source parse = do
  xs <- source
  mapM_ process xs
  return ()
  where
    process x = do
      t <- fromEnum . utSeconds <$> getUnixTime
      mapM (\(k,i) -> save acid k i) $ parse t x

parsePair :: Time -> String -> [(Key, Item)]
parsePair t xs =
  case words xs of
    [] -> []
    (k:e:[]) -> [(k,(e, 1, t))]
    _ -> []
