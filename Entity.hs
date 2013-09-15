{-# LANGUAGE RecordWildCards #-}
module Entity where

import Data.List (find)
import Data.IORef
import qualified Data.IntMap as IM
import Control.Monad.Reader

import GameTypes
import AI.Trigger

-- TODO Find a less obnoxious way to accomplish this
triggerCode :: Trigger -> Int
triggerCode (Tick) = 0
triggerCode (Impacted _) = 1
triggerCode (Pierced _) = 2
triggerCode (Slashed _) = 3
triggerCode (Burned _) = 4
triggerCode (Seen) = 5

aiByEID :: EID -> Game AI
aiByEID eid = do
  Entity{..} <- getByEID eid
  return ai

hpByEID :: EID -> Game Int
hpByEID eid = do
  Entity{..} <- getByEID eid
  return hp

bodyByEID :: EID -> Game Body
bodyByEID eid = do
  Entity{..} <- getByEID eid
  return body

respondTo :: EID -> Responder
respondTo eid t = do
  AI{..} <- aiByEID eid
  let t' = triggerCode t
  IM.findWithDefault ifMissing t' methods t

makeMethodMap :: [(Int, Responder)] -> TrigMap
makeMethodMap = IM.fromList

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
