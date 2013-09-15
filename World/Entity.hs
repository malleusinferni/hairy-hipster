{-# LANGUAGE RecordWildCards #-}
module World.Entity where

import Data.List (find, delete)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Control.Monad.Reader

import Support.Rand

import World.Core

import Entity.Core
import Entity.Body
import Entity.Species

storeEntity, updateEntity :: Entity -> Game ()
storeEntity e = entities %= (e:)
updateEntity e = entities %= ((e :) . delete e)

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
hpByEID eid = do
  Entity{..} <- getByEID eid
  return hp

bodyByEID :: EID -> Game Body
bodyByEID eid = do
  Entity{..} <- getByEID eid
  return body

randomSpecies :: Game Species
randomSpecies = do
  species <- asks speciesData
  Just soSueMe <- anyOf species
  return soSueMe

getEntities :: Game [Entity]
getEntities = asks entities >>= liftIO . readIORef

getEntitiesWhere :: (Entity -> Bool) -> Game [Entity]
getEntitiesWhere test = filter test `fmap` getEntities

getEntitiesNear :: Entity -> Game [Entity]
getEntitiesNear (Entity { location = here }) =
  getEntitiesWhere ((== here) . location)

getByEID :: EID -> Game Entity
getByEID anid = do
  Just self <- find ((== anid) . eid) `fmap` getEntities
  return self
