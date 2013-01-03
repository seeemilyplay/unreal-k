module InMemoryServer (
    InMemoryServer
  , withInMemoryServer
  , open
  , save
  , top
  , Storage.all
  , Slots
  , Time
  , Element
  , Key
  , Item
  , Count
  , K
  , Top) where

import Control.Concurrent.STM

import qualified Data.Map.Strict as Map

import Storage
import TopK

data InMemoryServer = InMemoryServer Slots K (Time -> Time) (TVar (Map.Map Key TopK))

withInMemoryServer :: Slots -> K -> (Time -> Time) -> (InMemoryServer -> IO a) -> IO a
withInMemoryServer s k pf f = open s k pf >>= f

open :: Slots -> K -> (Time -> Time) -> IO InMemoryServer
open s k pf = do
  m <- atomically $ newTVar Map.empty
  return $ InMemoryServer s k pf m

instance Storage InMemoryServer where
  save (InMemoryServer s _ pf tv) k x@(_,_,t) = atomically . modifyTVar' tv $ \m ->
    let v = case Map.lookup k m of
              Nothing -> create s k x
              (Just tk) -> add (pf t) x tk in
    Map.insert k v m

  top (InMemoryServer _ n _ tv) k = do
    m <- readTVarIO tv
    case Map.lookup k m of
      Nothing -> return (0, 0, 0, [])
      (Just tk) -> return $ topK n tk

  all (InMemoryServer _ n _ tv) = do
    m <- readTVarIO tv
    return . map (\(k, v) -> (k, topK n v)) $ Map.assocs m
