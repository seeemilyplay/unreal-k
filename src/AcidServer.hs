module AcidServer (
    AcidServer
  , withAcidServer
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
  , Item
  , Count
  , Top) where

import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map

import TopK

type Top = (Count,[(Element,Count)])

data All = All !(Map.Map Key TopK)
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''TopK)
$(deriveSafeCopy 0 'base ''All)

internalSave :: Slots -> Period -> Key -> Item -> Update All ()
internalSave s p k x = do
  All m <- get
  let v = case Map.lookup k m of
            Nothing -> create k p x
            (Just tk) -> increment s p x tk
  put . All $ Map.insert k v m

internalTop :: Key -> Query All Top
internalTop k = do
  All m <- ask
  case Map.lookup k m of
    Nothing -> return (0, [])
    (Just tk) -> return $ topK tk

internalAll :: Query All [(Key, Top)]
internalAll = do
  All m <- ask
  return .map (\(k, v) -> (k, topK v)) $ Map.assocs m

$(makeAcidic ''All ['internalAll, 'internalSave, 'internalTop])

data AcidServer = AcidServer Slots (Time -> Time) (AcidState All)

withAcidServer :: Slots -> (Time -> Time) -> (AcidServer -> IO a) -> IO a
withAcidServer s pf f = bracket (open s pf) close f

open :: Slots -> (Time -> Time) -> IO AcidServer
open s pf = do
  acid <- openLocalState (All Map.empty)
  return $ AcidServer s pf acid

save :: AcidServer -> Key -> Item -> IO ()
save (AcidServer s pf acid) k (e,c,t) = update acid (InternalSave s (pf t) k (e,c,t))

top :: AcidServer -> Key -> IO Top
top (AcidServer _ _ acid) k = query acid (InternalTop k)

all :: AcidServer -> IO [(Key, Top)]
all (AcidServer _ _ acid) = query acid InternalAll

close :: AcidServer -> IO ()
close (AcidServer _ _ acid) = closeAcidState acid

checkpoint :: AcidServer -> IO ()
checkpoint (AcidServer _ _ acid) = createCheckpoint acid
