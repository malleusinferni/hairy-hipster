module Action where

import Data.List (find)
import qualified Data.IntMap as IM

import GameTypes
import Describe
import Entity
import Coords
import Room

-- TODO Find a less obnoxious way to accomplish this
triggerCode :: Trigger -> Int
triggerCode (Tick) = 0
triggerCode (Impacted _) = 1
triggerCode (Pierced _) = 2
triggerCode (Slashed _) = 3
triggerCode (Burned _) = 4
triggerCode (Seen) = 5

getResponder :: Trigger -> IM.IntMap a -> Maybe a
getResponder = IM.lookup . triggerCode

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
  describe (See :& Agent _ : Patient p : _) =
    unwords [describe p, cverb p "be", "lurking in the darkness"]
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
