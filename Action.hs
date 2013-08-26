module Action where

import Data.List (find)

import Describe
import Entity
import Coords
import Room

-- Commands which an actor AI may issue in response to Tick
data Action = Attack -- Damage another entity
            | Eat -- Consume an entity (dead or living!)
            | Goto -- Actor goes to a different location
            | Take | Put -- Move stuff between world and inventory
            | Open | Close -- Door, chest, portal...?
            | Ask | Tell -- Includes reading and writing
            | Copulate -- Laying eggs???
            | Rest -- Do nothing (recuperate if possible)
            -- NOTE: Looking around doesn't consume a turn!
  deriving (Eq, Show)

-- Prompts to which an AI may (or may not) respond
data Trigger = Tick
             | Impacted Int
             | Pierced Int
             | Slashed Int
             | Burned Int
             | Seen
  deriving (Eq, Show)

-- Descriptive account of one result of an action
data Event = Outcome :& [EvArg]

-- Effect of the action on the patient (IMPORTANT!)
data Outcome = NothingHappens
             | TakeDamage
             | NearDeath
             | Stand
             | Walk
             | See
             | Die
             | Win
             | Lose
  deriving (Eq, Show)

-- Semantic arguments to an Outcome
data EvArg = Agent Entity
           | Patient Entity
           | Using Entity
           | ByAmount Int
           | Into Coords
           | OutOf Coords
           | WhichWay Cardinal
  deriving (Eq, Show)

-- Bind cause to effect
data EventReport = Trigger :=> [Event]

infixr 1 :=>
infixr 2 :&

subj, obj, poss :: Nominable a => a -> String
subj = nominative . name
obj = accusative . name
poss = genitive . name

cverb :: Nominable a => a -> String -> String
cverb a = conj a . verb

aeverb :: Nominable a => a -> Outcome -> String
aeverb a = cverb a . downcase . show

-- TODO Rewrite all of this to use randomness, vocabulary, etc.
instance Effable Event where
  describe (Walk :& (Agent a : WhichWay Up : _)) =
    unwords [subj a, cverb a "climb", "up the well"]
  describe (TakeDamage :& (Agent a : Patient p : ByAmount i : _)) =
    unwords [subj a, cverb a "strike", obj p, "for", show i, "damage"]
  describe (NearDeath :& Patient p : _)
    | isPlayer p = unwords [subj p, cverb p "feel", "woozy from blood loss"]
    | otherwise = unwords [subj p, cverb p "stagger", "under the blow"]
  describe (See :& Agent a : Patient p : _) =
    unwords [subj a, cverb a "see", describe p, "lurking in the darkness"]
  describe (Die :& Agent a : Patient p : _) =
    unwords [subj a, cverb a "defeat", obj p]
  describe (Die :& Patient p : _) =
    unwords [subj p, cverb p "perish"]
  describe (Win :& Patient p : _) =
    unwords [subj p, cverb p "emerge", "victorious"]
  describe (Lose :& Patient p : _) =
    unwords [subj p, cverb p "escape", "with", poss p, "life"]
  describe (v :& Agent a : Patient p : _) =
    unwords [subj a, aeverb a v, obj p]
  describe (v :& Agent a : _ ) = unwords [subj a, aeverb a v]
  describe (v :& Patient p : _) = unwords [subj p, aeverb p v]
  describe _ = "UNIMPLEMENTED"

instance Effable EventReport where
  describe (_ :=> [o1, NothingHappens :& (Agent a : Patient p : _)]) =
    unwords [describe o1, "to no effect"]
  describe (t :=> outcomes) = unsentence outcomes
