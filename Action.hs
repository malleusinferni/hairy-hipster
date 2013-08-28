module Action where

import GameTypes
import Describe
import Entity ()
import Coords

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
  describe (_ :& []) = "nothing happens"
  describe _ = "UNIMPLEMENTED"

instance Effable EventReport where
  describe (_ :=> [o1, NothingHappens :& (Agent _ : Patient _ : _)]) =
    unwords [describe o1, "to no effect"]
  describe (_ :=> outcomes) = unsentence outcomes
