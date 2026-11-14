module Data.Tacview.MinId (
    minId
) where

import Control.Concurrent.Channel (Channel)
import Control.Concurrent.Channel.Split
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict qualified as HashMap
import Data.Tacview
import GHC.Data.Word64Map.Strict qualified as WM
import GHC.Data.Word64Set qualified as S

data MinIdState = MinIdState {
    -- A Word64Map was ~7x faster than a BasicHashTable for these lookups
    -- (only a few thousand IDs, and almost every call is a lookup, not an insert).
    idMap :: !(TacIdMap TacId),
    nextId :: !TacId
}

minId :: Channel c => Reader c ParsedLine -> Writer c ParsedLine -> IO ()
minId source sink = void $ stateConsumeChannel source (MinIdState WM.empty 1) $ \s l -> do
    let (l', s') = runState (mapLine l) s
    atomically $ writeChannel' sink l'
    pure s'

mapLine :: ParsedLine -> State MinIdState ParsedLine
mapLine (PropLine i p) = do
    mappedId <- lookupOrInsert i
    -- Most properties don't reference other objects, so don't rebuild the whole map.
    -- The fold is strict so it doesn't build a thunk for each property,
    -- and we reverse it to hand out new IDs in the same order as traverse.
    let refs = reverse $ HashMap.foldlWithKey' (\acc k v -> case v of
            Referencing r -> (k, r) : acc
            _ -> acc) [] p
        -- If we come across a property (say, LockedTarget) referencing a previously-unseen ID,
        -- assume we'll run into it soon and make a mapping for it.
        remap m (k, r) = do
            r' <- lookupOrInsert r
            pure $ if r' == r then m else HashMap.insert k (Referencing r') m
    mappedProps <- foldM remap p refs
    pure $ PropLine mappedId mappedProps
-- If we see a removal line with an ID we haven't seen before, that's weird,
-- but let's at least make it small.
mapLine (RemLine i) = RemLine <$> lookupOrInsert i
mapLine (EventLine t is p) = do
    mis <- mapM lookupOrInsert $ S.toList is
    pure $ EventLine t (S.fromList mis) p
mapLine l = pure l

lookupOrInsert :: TacId -> State MinIdState TacId
lookupOrInsert 0 = pure 0
lookupOrInsert i = do
    s <- get
    case WM.lookup i s.idMap of
        Just m -> pure m
        Nothing -> do
            put $! MinIdState (WM.insert i s.nextId s.idMap) (s.nextId + 1)
            pure s.nextId
