{-# LANGUAGE RecordWildCards #-}
module World.Entity where

import Data.List (find)
import Data.IORef
import Control.Monad.Reader

import World.Core

import Entity.Core
import Entity.Body

hpByEID :: EID -> Game Int
hpByEID eid = do
  Entity{..} <- getByEID eid
  return hp

bodyByEID :: EID -> Game Body
bodyByEID eid = do
  Entity{..} <- getByEID eid
  return body

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
