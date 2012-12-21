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
  , Top) where

import Control.Concurrent.STM

import qualified Data.Map.Strict as Map

import Storage
import TopK

data InMemoryServer = InMemoryServer Slots (Time -> Time) (TVar (Map.Map Key TopK))

withInMemoryServer :: Slots -> (Time -> Time) -> (InMemoryServer -> IO a) -> IO a
withInMemoryServer s pf f = open s pf >>= f

open :: Slots -> (Time -> Time) -> IO InMemoryServer
open s pf = do
  m <- atomically $ newTVar Map.empty
  return $ InMemoryServer s pf m

instance Storage InMemoryServer where
  save (InMemoryServer s pf tv) k x@(_,_,t) = atomically . modifyTVar' tv $ \m ->
    let v = case Map.lookup k m of
              Nothing -> create k (pf t) x
              (Just tk) -> increment s (pf t) x tk in
    Map.insert k v m

  top (InMemoryServer _ _ tv) k = do
    m <- readTVarIO tv
    case Map.lookup k m of
      Nothing -> return (0, [])
      (Just tk) -> return $ topK tk

  all (InMemoryServer _ _ tv) = do
    m <- readTVarIO tv
    return . map (\(k, v) -> (k, topK v)) $ Map.assocs m
