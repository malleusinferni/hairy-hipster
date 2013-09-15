module Entity.Core where

import Describe

import Support.Coords
import Support.Measure

import Entity.Body
import Entity.Species

-- An object in the game, usually with a physical body
data Entity = Entity
  { eid :: EID
  , hp :: Int
  , body :: Body
  , isPlayer :: Bool
  , location :: Coords
  , species :: Species
  , power :: Int
  } deriving (Show)

-- Entities are indexed by their unique IDs
type EID = Int

instance Eq Entity where
  Entity { eid = lhs } == Entity { eid = rhs } = lhs == rhs

instance Ord Entity where
  compare (Entity { eid = lhs }) (Entity { eid = rhs }) = compare lhs rhs

instance Nominable Entity where
  name a | isPlayer a = noun You
  name (Entity { species = s })= noun (The s)

instance Effable Entity where
  describe e = nominative $ noun subj
    where subj = An $ Adj howtall whatspecies
          howtall = unwords [numWord inFeet, "foot tall"]
          whatspecies = species e
          inFeet = size (body e) `rdiv` 12

isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= maxHPFor (body e) `rdiv` 5
isAlive = (> 0) . hp
isDead = not . isAlive

isOpponentOf :: Entity -> Entity -> Bool
isOpponentOf self e =
  isAlive e &&
    eid e /= eid self &&
      location e == location self
