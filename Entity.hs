{-# LANGUAGE RecordWildCards #-}
module Entity where

import Data.List (find)
import Data.IORef
import Control.Monad.Reader

import GameTypes

import Entity.Body
import Entity.Species

hpByEID :: EID -> Game Int
hpByEID eid = do
  Entity{..} <- getByEID eid
  return hp

bodyByEID :: EID -> Game Body
bodyByEID eid = do
  Entity{..} <- getByEID eid
  return body

-- Size in inches
sizeRangeFor :: Species -> (Int, Int)
sizeRangeFor species = (minHeight species, maxHeight species)

inFeet :: Entity -> Int
inFeet e = size (body e) `rdiv` 12

rdiv :: Int -> Int -> Int
rdiv q d = round $ toRational q / toRational d

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

maxHPFor, strengthFor :: Body -> Int
maxHPFor body = size body `rdiv` 3
strengthFor body = size body `rdiv` 5

isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= maxHPFor (body e) `rdiv` 5
isAlive = (> 0) . hp
isDead = not . isAlive

isOpponentOf :: Entity -> Entity -> Bool
isOpponentOf self e =
  isAlive e &&
    eid e /= eid self &&
      location e == location self
