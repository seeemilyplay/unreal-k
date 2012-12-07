module TopK ( Slots
            , Time
            , Count
            , Element
            , Key
            , Item
            , TopK(..)
            , empty
            , topK
            , increment) where

import Data.List
import Data.Ord
import Data.Typeable


type Slots = Int
type Time = Int
type Count = Int
type Element = String
type Key = String
type Item = (Element, Count, Time)

data TopK = TopK Key [Item] [Item]
  deriving (Show, Typeable)

empty :: Key -> TopK
empty k = TopK k [] []

topK :: TopK -> [Item]
topK (TopK _ _ xs) = sortBy comparingCount xs

increment :: Maybe Slots -> Item -> TopK -> TopK
increment s x (TopK k xs ys) =
  let (xs', ys') = agreeAndCull s (add x xs) ys in
  TopK k xs' ys'

add :: Item -> [Item] -> [Item]
add x xs = updateOrInsert x xs $ \c origc -> c + origc

agreeAndCull :: Maybe Slots -> [Item] -> [Item] -> ([Item], [Item])
agreeAndCull s xs ys =
  let ys' = foldr shunt ys xs
      xs' = foldr washBack xs ys'
  in (cullBy comparingTime s xs', cullBy comparingCount s ys')

shunt :: Item -> [Item] -> [Item]
shunt x xs = updateOrInsert x xs $ \c origc -> max c origc

washBack :: Item -> [Item] -> [Item]
washBack x xs = update x xs $ \c origc -> if origc < c then c + origc else c

update :: Item -> [Item] -> (Count -> Count -> Count) -> [Item]
update (e, c, t) xs combine =
  case partition (\(e', _, _) -> e'==e) xs of
    ([], xs') -> xs'
    ((_, c', t'):[], xs') -> (e, combine c c', max t t') : xs'
    _ -> error "can't update, more than one matching element"

updateOrInsert :: Item -> [Item] -> (Count -> Count -> Count) -> [Item]
updateOrInsert (e, c, t) xs combine =
  case partition (\(e', _, _) -> e'==e) xs of
    ([], xs') -> (e, c, t) : xs'
    ((_, c', t'):[], xs') -> (e, combine c c', max t t') : xs'
    _ -> error "can't update or insert, more than one matching element"

cullBy :: (a -> a -> Ordering) -> Maybe Int -> [a] -> [a]
cullBy comp (Just s) xs = take s $ sortBy comp xs
cullBy _ Nothing xs = xs

comparingCount :: Item -> Item -> Ordering
comparingCount = comparing (\(_, c, _) -> (-1) * c)

comparingTime :: Item -> Item -> Ordering
comparingTime = comparing (\(_, _, t) -> (-1) * t)
