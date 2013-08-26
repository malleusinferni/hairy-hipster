module Entity where

import Describe
import Coords
import Room

data Entity = Entity {
    eid :: EID,
    ai :: AI,
    hp :: Int,
    location :: Room,
    species :: Species,
    power :: Int
  } deriving (Show)

instance Eq Entity where
  Entity { eid = lhs } == Entity { eid = rhs } = lhs == rhs

instance Ord Entity where
  compare (Entity { eid = lhs }) (Entity { eid = rhs }) = compare lhs rhs

instance Nominable Species where
  name s = Noun (describe s) (describe s ++ "'s") (describe s) False

instance Effable Species where
  describe Merovingian = "Merovingian"
  describe s = downcase (show s)

instance Nominable Entity where
  name actor | aiType (ai actor) == Player = noun You
  name (Entity { species = s })= noun (The s)

instance Effable Entity where
  describe e = unwords [subj, "with", show (hp e), "HP"]
    where subj = nominative . noun . An $ species e

data Species = Shoggoth | Goblin | Unseelie | Merovingian
  deriving (Eq, Show, Ord, Enum, Bounded)

type EID = Int

data AIType = Player -- Controlled by IO hooks
            | Actor -- Controlled by runAI
            | Reactor -- Only responds to triggers
            | Inert -- No special behavior
  deriving (Eq, Show, Ord)

data AI = AI { aiType :: AIType, entity :: EID }
  deriving (Eq, Show)

hpRangeFor Goblin = (5, 25)
hpRangeFor Merovingian = (15, 35)
hpRangeFor Shoggoth = (25, 45)
hpRangeFor _ = (10, 30)

strRangeFor Shoggoth = (10, 20)
strRangeFor _ = (5, 15)

isActor = (`elem` [Player, Actor]) . aiType . ai

isPlayer = (== Player) . aiType . ai

stillAlive = (> 0) . hp
