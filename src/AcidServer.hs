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

import Storage
import TopK

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

withAcidServer :: Bool -> Slots -> (Time -> Time) -> (AcidServer -> IO a) -> IO a
withAcidServer persist s pf f = bracket (open persist s pf) close f

open :: Bool -> Slots -> (Time -> Time) -> IO AcidServer
open persist s pf = do
  acid <- if persist
             then openLocalState (All Map.empty)
             else openMemoryState (All Map.empty)
  return $ AcidServer s pf acid

checkpoint :: AcidServer -> IO ()
checkpoint (AcidServer _ _ acid) = createCheckpoint acid

close :: AcidServer -> IO ()
close (AcidServer _ _ acid) = closeAcidState acid

instance Storage AcidServer where
  save (AcidServer s pf acid) k (e,c,t) =
    update acid (InternalSave s (pf t) k (e,c,t))

  top (AcidServer _ _ acid) k =
    query acid (InternalTop k)

  all (AcidServer _ _ acid) =
    query acid InternalAll
