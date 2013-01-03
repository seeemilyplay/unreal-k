module AcidServer (
    AcidServer
  , withAcidServer
  , open
  , save
  , top
  , Storage.all
  , close
  , checkpoint
  , Slots
  , Time
  , Element
  , Key
  , Item
  , Count
  , Top) where

import Data.Acid
import Data.Acid.Memory
import Data.SafeCopy
import Data.Typeable
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map

import Config
import Storage
import TopK

data All = All !(Map.Map Key TopK)
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''Entries)
$(deriveSafeCopy 0 'base ''TopK)
$(deriveSafeCopy 0 'base ''All)

internalSave :: Slots -> Period -> Key -> Item -> Update All ()
internalSave s p k x = do
  All m <- get
  let v = case Map.lookup k m of
            Nothing -> create s k x
            (Just tk) -> add p x tk
  put . All $ Map.insert k v m

internalTop :: K -> Key -> Query All Top
internalTop n k = do
  All m <- ask
  case Map.lookup k m of
    Nothing -> return (0, 0, 0, [])
    (Just tk) -> return $ topK n tk

internalAll :: K -> Query All [(Key, Top)]
internalAll n = do
  All m <- ask
  return .map (\(k, v) -> (k, topK n v)) $ Map.assocs m

$(makeAcidic ''All ['internalAll, 'internalSave, 'internalTop])

data AcidServer = AcidServer Config (AcidState All)

withAcidServer :: Config -> (AcidServer -> IO a) -> IO a
withAcidServer c f = bracket (open c) close f

open :: Config -> IO AcidServer
open c = do
  acid <- if (cfgpersist c)
             then openLocalState (All Map.empty)
             else openMemoryState (All Map.empty)
  return $ AcidServer c acid

checkpoint :: AcidServer -> IO ()
checkpoint (AcidServer _ acid) = createCheckpoint acid

close :: AcidServer -> IO ()
close (AcidServer _ acid) = closeAcidState acid

instance Storage AcidServer where
  save (AcidServer c acid) k it@(_,_,t) =
    update acid (InternalSave (cfgslots c) ((cfgperiod c) t) k it)

  top (AcidServer c acid) k =
    query acid (InternalTop (cfgk c) k)

  all (AcidServer c acid) =
    query acid (InternalAll (cfgk c))
