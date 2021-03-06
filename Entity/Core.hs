{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Entity.Core where

import Data.Function (on)

import Describe

import Support.Measure
import Support.Coords

import Entity.Trait (get, replace, Map)
import qualified Entity.Trait as K
import Entity.Value
import Entity.Body
import Entity.Species

-- An object in the game, usually with a physical body
data Entity = Entity
  { eid :: !EID
  , body :: !Body
  , traits :: !Map
  } deriving (Show)

location :: Entity -> Coords
location (Entity{..}) = coordsValue (get K.Location traits)

species :: Entity -> Species
species (Entity{..}) = speciesValue (get K.Species traits)

isPlayer :: Entity -> Bool
isPlayer (Entity{..}) = boolValue (get K.IsPlayer traits)

hp, power, speed :: Entity -> Int
hp (Entity{..}) = intValue (get K.HitPoints traits)
power (Entity{..}) = intValue (get K.Strength traits)
speed (Entity{..}) = intValue (get K.Dexterity traits)

(#=) :: (Monad m) => (Entity, K.Key) -> Value -> m Entity
(self, k) #= v = return $ self { traits = new }
  where err = error msg
        msg = unwords ["Type mismatch:", show v, "vs", show (get k old)]
        old = traits self
        new = replace k v old err

-- Entities are indexed by their unique IDs
type EID = Int

instance Eq Entity where
  (==) = (==) `on` eid

instance Ord Entity where
  compare = compare `on` eid

instance Effable Entity where
  describe = the . whatkind
    where whatkind e
            | isPlayer e = [word "player"]
            | otherwise = describe (species e)

isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= maxHPFor (body e) `rdiv` 5
isAlive = (> 0) . hp
isDead = not . isAlive

isOpponentOf :: Entity -> Entity -> Bool
isOpponentOf self e =
  isAlive e &&
    eid e /= eid self &&
      location e == location self
