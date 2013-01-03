module Storage (
    Storage
  , save
  , top
  , Storage.all
  , Slots
  , Time
  , Element
  , Key
  , Item
  , Count
  , Top) where

import TopK

type Top = (Count,Time,Time,[(Element,Count)])

class Storage a where

  save :: a -> Key -> Item -> IO ()

  top :: a -> Key -> IO Top

  all :: a -> IO [(Key, Top)]