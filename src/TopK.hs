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
import Data.Ord
import Data.Typeable

type Slots = Int
type Period = Time
type Key = String
type Element = String
type Time = Int
type Count = Int
type Item = (Element, Count, Time)


data Entry = Entry {
    eelement :: Element
  , efrom :: Time
  , eto :: Time
  , ecount :: Count
} deriving (Show, Typeable)

data TopK = TopK {
    tkkey :: Key
  , tkfrom :: Time
  , tkto :: Time
  , tkcount :: Count
  , tkrecent :: [Entry]
  , tktop :: [Entry]
} deriving (Show, Typeable)

topK :: TopK -> (Count,[(Element,Count)])
topK tk = (tkcount tk, map (\e -> (eelement e, ecount e)) $ tktop tk)

create :: Key -> Item -> TopK
create k (e, c, t) =
  TopK {
    tkkey = k
  , tkfrom = t
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

class Addable a b where
  add :: a -> b -> b

instance Addable Entry TopK where
  add e tk =
    let tkrecent' = add e (tkrecent tk)
        tktop' = add e (tktop tk) in
    tk {
      tkfrom = min (efrom e) (tkfrom tk)
    , tkto = max (eto e) (tkto tk)
    , tkrecent = tkrecent'
    , tktop = sortBy (comparing ((*) (-1) . ecount)) $ add (head tkrecent') tktop'
    }

instance Addable Entry [Entry] where
  add e es =
    case find ((==) (eelement e) . eelement) es of
      Nothing -> e : es
      Just e' -> (add e e') : filter ((/=) (eelement e) . eelement) es

instance Addable Entry Entry where
  add e e' | (eelement e) == (eelement e') = e {
      efrom = min (efrom e) (efrom e')
    , eto = max (eto e) (eto e')
    , ecount = (ecount e) + (ecount e')
    }
  add _ _ = error "elements don't match"

class Mergeable a b where
  merge :: a -> b -> b

instance Mergeable Entry [Entry] where
  merge e es =
    case find ((==) (eelement e) . eelement) es of
      Nothing -> e : es
      Just e' -> (merge e e') : filter ((/=) (eelement e) . eelement) es

instance Mergeable Entry Entry where
  merge e e' | (eelement e) == (eelement e') = e {
      efrom = min (efrom e) (efrom e')
    , eto = max (eto e) (eto e')
    , ecount = max (ecount e) (ecount e')
    }
  merge _ _ = error "elements don't match"

class Timed a where
  scale :: Time -> Time -> a -> a

instance (Timed a) => Timed [a] where
  scale f t = map (scale f t)

instance Timed TopK where
  scale f t tk | f <= (tkfrom tk) = tk {
      tkto = max t (tkto tk)
    }
  scale f t tk =
    let x = (tkto tk - f) * (tkcount tk)
        y = tkto tk - tkfrom tk
        t' = max t (tkto tk) in
    tk {
      tkfrom = f
    , tkto = t'
    , tkcount = round $ (fromIntegral x :: Double) / (fromIntegral y :: Double)
    , tkrecent = scale f t' (tkrecent tk)
    , tktop = scale f t' (tktop tk)
    }

instance Timed Entry where
  scale f _ e | f <= (efrom e) = e
  scale f _ e =
    let x = (eto e - f) * (ecount e)
        y = eto e - efrom e in
    e {
    efrom = f
  , ecount = round $ (fromIntegral x :: Double) / (fromIntegral y :: Double)
  }
