{-# LANGUAGE FlexibleInstances #-}
module Action where

import GameTypes
import Describe
import Entity ()
import Coords

instance Effable Corridor where
  describe c = "the " ++ doorName c

instance Effable Cardinal where
  describe = downcase . show

instance Effable [String] where
  describe = unwords

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
  describe (See :& Agent _ : Via d : OutOf h : WhichWay c : _) =
    unwords [describe d, "leads", describe c]
  describe (Die :& Agent a : Patient p : _) =
    unwords [subj a, cverb a "defeat", obj p]
  describe (Die :& Patient p : _) =
    unwords [subj p, cverb p "perish"]
  describe (Win :& Patient p : _) =
    unwords [subj p, cverb p "emerge", "victorious"]
  describe (Lose :& Patient p : _) =
    unwords [subj p, cverb p "escape", "with", poss p, "life"]
  describe (Fail :& Tried v : _) =
    unwords ["there's nothing to", downcase (show v)]
  describe (Heal :& Patient p : _) =
    unwords [subj p, cverb p "recover", "a little health"]
  describe (Walk :& Patient p : WhichWay u : Via d : _)
    | u `elem` [Up, Down] =
      unwords [subj p, cverb p "climb", describe u, describe d]
    | otherwise = unwords [subj p, cverb p "go", "through", describe d]
  describe (Walk :& Patient p : Into d : _) = description d
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
