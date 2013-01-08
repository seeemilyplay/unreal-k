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

import Control.Arrow
import Control.Concurrent.STM

import qualified Data.Map.Strict as Map

import Config
import Storage
import TopK

data InMemoryServer = InMemoryServer Config (TVar (Map.Map Key TopK))

withInMemoryServer :: Config -> (InMemoryServer -> IO a) -> IO a
withInMemoryServer c f = open c >>= f

open :: Config -> IO InMemoryServer
open c = do
  m <- atomically $ newTVar Map.empty
  return $ InMemoryServer c m

instance Storage InMemoryServer where
  save (InMemoryServer c tv) k x@(_,_,t) = atomically . modifyTVar' tv $ \m ->
    let v = case Map.lookup k m of
              Nothing -> create (cfgslots c) k x
              (Just tk) -> add (cfgperiod c t) x tk in
    Map.insert k v m

  top (InMemoryServer c tv) k = do
    m <- readTVarIO tv
    case Map.lookup k m of
      Nothing -> return (0, 0, 0, [])
      (Just tk) -> return $ topK (cfgk c) tk

  all (InMemoryServer c tv) = do
    m <- readTVarIO tv
    return . map (second (topK (cfgk c))) $ Map.assocs m
