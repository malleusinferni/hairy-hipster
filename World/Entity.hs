{-# LANGUAGE RecordWildCards #-}
module World.Entity where

import Data.List (find, delete)
import Data.Maybe (fromJust)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Control.Monad.Reader

import Support.Rand

import World.Core

import Entity.Core
import Entity.Body
import Entity.Species
import Entity.Value
import qualified Entity.Trait as T

storeEntity :: Entity -> Game ()
storeEntity e = entities %= (e:)

updateEntity :: Entity -> T.Key -> (Value -> Value) -> Game ()
updateEntity eo k f = do
  let vo = T.get k (traits eo)
      vn = f vo
  en <- (eo, k) #= vn
  en `seq` entities %= ((en :) . delete eo)

putAI :: EID -> AI -> Game ()
putAI eid ai = bindings %= IM.insert eid ai

getAI :: EID -> Game AI
getAI eid = do
  ref <- asks bindings
  ais <- liftIO $ readIORef ref
  let err = error (unlines [e1, e2])
      e1 = "Tried to look up AI for missing EID: " ++ show eid
      e2 = "Map contents were: " ++ show ais
  maybe err return $ IM.lookup eid ais

hpByEID :: EID -> Game Int
hpByEID eid = hp `fmap` getByEID eid

bodyByEID :: EID -> Game Body
bodyByEID eid = body `fmap` getByEID eid

randomSpecies :: Game Species
randomSpecies = fromJust `fmap` (anyOf =<< asks speciesData)

getEntities :: Game [Entity]
getEntities = asks entities >>= liftIO . readIORef

getEntitiesWhere :: (Entity -> Bool) -> Game [Entity]
getEntitiesWhere test = filter test `fmap` getEntities

getEntitiesNear :: Entity -> Game [Entity]
getEntitiesNear e =
  let here = location e in
    getEntitiesWhere ((== here) . location)

getByEID :: EID -> Game Entity
getByEID anid = fromJust `fmap` find ((== anid) . eid) `fmap` getEntities
