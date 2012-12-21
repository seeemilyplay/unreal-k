{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module TopK ( Slots
            , Period
            , Key
            , Element
            , Time
            , Count
            , Item
            , Entry(..)
            , TopK(..)
            , topK
            , create
            , increment) where

import Data.List
import Data.Typeable

type Slots = Int
type Period = Time
type Key = String
type Element = String
type Time = Int
type Count = Int
type Item = (Element, Count, Time)


data Entry = Entry {
    eelement :: !Element
  , efrom :: !Time
  , eto :: !Time
  , ecount :: !Count
} deriving (Show, Typeable)

data TopK = TopK {
    tkkey :: !Key
  , tkfrom :: !Time
  , tkto :: !Time
  , tkcount :: !Count
  , tkrecent :: ![Entry]
  , tktop :: ![Entry]
} deriving (Show, Typeable)

topK :: TopK -> (Count,[(Element,Count)])
topK tk = (tkcount tk, map (\e -> (eelement e, ecount e)) $ tktop tk)

create :: Key -> Period -> Item -> TopK
create k p (e, c, t) =
  TopK {
    tkkey = k
  , tkfrom = p
  , tkto = t
  , tkcount = c
  , tkrecent = [en]
  , tktop = [en]
  }
  where
    en = Entry {
      eelement = e
    , efrom = t
    , eto = t
    , ecount = c
    }

increment :: Slots -> Period -> Item -> TopK -> TopK
increment _ _ (_, _, t) tk | t < tkfrom tk = tk
increment s p (e, c, t) tk =
  (cull s . add en $ scale p t tk) {
    tkcount = (tkcount tk) + c
  }
  where
    en = Entry {
      eelement = e
    , efrom = t
    , eto = t
    , ecount = c
    }

cull :: Slots -> TopK -> TopK
cull s tk =
  tk {
    tkrecent = take s (tkrecent tk)
  , tktop = take s (tktop tk)
  }

add :: Entry -> TopK -> TopK
add e tk =
  let tkrecent' = addToEntries e (tkrecent tk)
      tktop' = addToEntries e (tktop tk) in
  tk {
    tkfrom = min (efrom e) (tkfrom tk)
  , tkto = max (eto e) (tkto tk)
  , tkrecent = tkrecent'
  , tktop = sortBy topKOrdering $ merge tkrecent' tktop'
  }

topKOrdering :: Entry -> Entry -> Ordering
topKOrdering e1 e2 =
  case compare (ecount e2) (ecount e1) of
    EQ -> compare (eelement e1) (eelement e2)
    x -> x

addToEntries :: Entry -> [Entry] -> [Entry]
addToEntries e es =
  case find ((==) (eelement e) . eelement) es of
    Nothing -> e : es
    Just e' -> (addEntries e e') : filter ((/=) (eelement e) . eelement) es

addEntries :: Entry -> Entry -> Entry
addEntries e e' | (eelement e) == (eelement e') = e {
    efrom = min (efrom e) (efrom e')
  , eto = max (eto e) (eto e')
  , ecount = (ecount e) + (ecount e')
  }
addEntries _ _ = error "elements don't match"

merge :: [Entry] ->[Entry] -> [Entry]
merge es es' = foldl (flip mergeIntoEntries) es' es

mergeIntoEntries :: Entry -> [Entry] -> [Entry]
mergeIntoEntries e es =
  case find ((==) (eelement e) . eelement) es of
    Nothing -> e : es
    Just e' -> (mergeEntries e e') : filter ((/=) (eelement e) . eelement) es

mergeEntries :: Entry -> Entry -> Entry
mergeEntries e e' | (eelement e) == (eelement e') = e {
    efrom = min (efrom e) (efrom e')
  , eto = max (eto e) (eto e')
  , ecount = max (ecount e) (ecount e')
  }
mergeEntries _ _ = error "elements don't match"

scale :: Time -> Time -> TopK -> TopK
scale f t tk | f <= (tkfrom tk) = tk {
    tkfrom = f
  , tkto = max t (tkto tk)
  }
scale f t tk =
  let x = (tkto tk - f) * (tkcount tk)
      y = tkto tk - tkfrom tk
      t' = max t (tkto tk) in
  tk {
    tkfrom = f
  , tkto = t'
  , tkcount = round $ (fromIntegral x :: Double) / (fromIntegral y :: Double)
  , tkrecent = scaleEntries f $ tkrecent tk
  , tktop = scaleEntries f $ tktop tk
  }

scaleEntries :: Time -> [Entry] -> [Entry]
scaleEntries f = filter ((/=) 0 . ecount) . map (scaleEntry f)

scaleEntry :: Time -> Entry -> Entry
scaleEntry f e | f <= (efrom e) = e
scaleEntry f e =
  let x = (eto e - f) * (ecount e)
      y = eto e - efrom e in
  e {
  efrom = f
, ecount = round $ (fromIntegral x :: Double) / (fromIntegral y :: Double)
}
