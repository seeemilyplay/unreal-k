module AcidServer (
    AcidServer
  , open
  , save
  , top
  , AcidServer.all
  , close
  , checkpoint
  , Slots
  , Time
  , Element
  , Key
  , Item) where

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map

import TopK


data All = All !(Map.Map Element TopK)
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''TopK)
$(deriveSafeCopy 0 'base ''All)

internalSave :: Maybe Slots -> Key -> Item -> Update All ()
internalSave s k x = do
  All m <- get
  let v = increment s x $ Map.findWithDefault (TopK.empty k) k m
  put . All $ Map.insert k v m

internalTop :: Key -> Query All [Item]
internalTop k = do
  All m <- ask
  return . topK $ Map.findWithDefault (TopK.empty k) k m

internalAll :: Query All [(Key, [Item])]
internalAll = do
  All m <- ask
  return .map (\(k, v) -> (k, topK v)) $ Map.assocs m

$(makeAcidic ''All ['internalAll, 'internalSave, 'internalTop])

data AcidServer = AcidServer (Maybe Slots) (AcidState All)

open :: Maybe Slots -> IO AcidServer
open s = do
  acid <- openLocalState (All Map.empty)
  return $ AcidServer s acid

save :: AcidServer -> Key -> Item -> IO ()
save (AcidServer s acid) k x = update acid (InternalSave s k x)

top :: AcidServer -> Key -> IO [Item]
top (AcidServer _ acid) k = query acid (InternalTop k)

all :: AcidServer -> IO [(Key, [Item])]
all (AcidServer _ acid) = query acid InternalAll

close :: AcidServer -> IO ()
close (AcidServer _ acid) = closeAcidState acid

checkpoint :: AcidServer -> IO ()
checkpoint (AcidServer _ acid) = createCheckpoint acid
