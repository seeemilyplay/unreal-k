{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module Entries(
    Slots
  , Element
  , Time
  , Count
  , K
  , Entry(..)
  , Entries(..)
  , empty
  , add
  , scale
  , scaleCount
  , topK
  ) where

import Data.Maybe
import Data.Typeable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Slots = Int
type Element = String
type Time = Int
type Count = Int
type K = Int

data Entry = Entry {
    element :: !Element
  , from :: !Time
  , to :: !Time
  , count :: !Count
} deriving (Show, Typeable)

data Entries = Entries {
    slots :: !Slots
  , recent :: !(Set.Set (Time, Element))
  , large :: !(Set.Set (Count, Element))
  , entries :: !(Map.Map Element Entry)
} deriving (Show, Typeable)

empty :: Slots -> Entries
empty s = Entries {
    slots = s
  , recent = Set.empty
  , large = Set.empty
  , entries = Map.empty
  }

add :: Entry -> Entries -> Entries
add e es =
  case Map.lookup (element e) (entries es) of
    Just e' ->
      insert Entry {
        element = element e
      , from = min (from e) (from e')
      , to = max (to e) (to e')
      , count = count e + count e'
      } (delete e' es)
    Nothing -> insert e es

insert :: Entry -> Entries -> Entries
insert e es =
  let (moldest, recent') = boundedInsert (to e, element e) $ recent es
      (msmallest, large') = boundedInsert (count e, element e) $ large es
      entries' = Map.insert (element e) e $ entries es
  in Entries {
    slots = slots es
  , recent = recent'
  , large = large'
  , entries =
      case (moldest >>= flip Map.lookup entries' . snd,
            msmallest >>= flip Map.lookup entries' . snd) of
        (Just oldEntry, _) | Set.notMember (count oldEntry, element oldEntry) large' -> Map.delete (element oldEntry) entries'
        (_, Just smallEntry) | Set.notMember (to smallEntry, element smallEntry) recent' -> Map.delete (element smallEntry) entries'
        _ -> entries'
  }
  where
    boundedInsert :: (Ord a, Show a) => a -> Set.Set a -> (Maybe a, Set.Set a)
    boundedInsert x xs | Set.size xs < slots es = (Nothing, Set.insert x xs)
    boundedInsert x xs =
      case Set.findMin xs of
        minx | x > minx -> (Just minx, Set.insert x $ Set.delete minx xs)
        _ -> (Just x, xs)

delete :: Entry -> Entries -> Entries
delete e es = Entries {
    slots = slots es
  , recent = Set.delete (to e, element e) $ recent es
  , large = Set.delete (count e, element e) $ large es
  , entries = Map.delete (element e) $ entries es
  }

scale :: Time -> Entries -> Entries
scale f es = Map.foldr scaleEntry es (entries es)
  where
    scaleEntry :: Entry -> Entries -> Entries
    scaleEntry e es' | f <= from e = es'
    scaleEntry e es' = insert  Entry {
        element = element e
      , from = f
      , to = to e
      , count = scaleCount f (from e) (to e) (count e)
      } (delete e es')

-- utility function for scaling counts to time
scaleCount :: Time -> Time -> Time -> Count -> Count
scaleCount newfrom oldfrom theto oldcount =
  let f' = fromIntegral oldfrom :: Integer
      t = fromIntegral theto :: Integer
      f = fromIntegral newfrom :: Integer
      c' = fromIntegral oldcount :: Integer
      x = (t - f) * c'
      y = t - f'
  in round $ (fromIntegral x :: Double) / (fromIntegral y :: Double)

topK :: K -> Entries -> [Entry]
topK k es = take k . mapMaybe (flip Map.lookup (entries es) . snd) . reverse . Set.toAscList $ large es