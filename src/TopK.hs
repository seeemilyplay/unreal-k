{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}
module TopK ( Period
            , Key
            , Item
            , E.Slots
            , E.Element
            , E.Time
            , E.Count
            , E.K
            , E.Entry(..)
            , E.Entries(..)
            , TopK(..)
            , topK
            , create
            , add) where

import Data.Typeable
import qualified Entries as E

type Period = E.Time
type Key = String
type Item = (E.Element, E.Count, E.Time)

data TopK = TopK {
    key :: !Key
  , tkfrom :: !E.Time
  , tkto :: !E.Time
  , tkcount :: !E.Count
  , tkentries :: !E.Entries
} deriving (Show, Typeable)

topK :: E.K -> TopK -> (E.Count,E.Time,E.Time,[(E.Element,E.Count)])
topK k tk = (tkcount tk, tkfrom tk, tkto tk, map (\e -> (E.element e, E.count e)) . E.topK k $ tkentries tk)

toEntry :: Item -> E.Entry
toEntry (e, c, t) = E.Entry {
    E.element = e, E.from = t, E.to = t, E.count = c
  }

create :: E.Slots -> Key -> Item -> TopK
create s k it@(_, c, t) =
  TopK {
    key = k
  , tkfrom = t
  , tkto = t
  , tkcount = c
  , tkentries = E.add (toEntry it) (E.empty s)
  }

add :: Period -> Item -> TopK -> TopK
add p it@(_, c, t) tk = scale p $ TopK {
    key = key tk
  , tkfrom = min t (tkfrom tk)
  , tkto = max t (tkto tk)
  , tkcount = (tkcount tk) + c
  , tkentries = E.add (toEntry it) (tkentries tk)
  }

lag :: E.Time
lag = 10 * 60 * 1000 -- ten minutes

scale :: E.Time -> TopK -> TopK
scale f tk | tkfrom tk + lag < f = TopK {
    key = key tk
  , tkfrom = f
  , tkto = tkto tk
  , tkcount = round $ (fromIntegral ((tkto tk - f) * (tkcount tk)) :: Double) /
                         (fromIntegral (tkto tk - tkfrom tk) :: Double)
  , tkentries = E.scale f $ tkentries tk
  }
scale _ tk = tk
