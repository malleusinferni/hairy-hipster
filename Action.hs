module Action where

import Data.List (find)

import Describe
import Entity

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

data ActionEvent = VI Action Entity
                 | VT Action Entity Entity
                 | V3 Action Entity Entity Entity

data Outcome = TakeDamage Entity Int
             | NearDeath Entity
             | Die Entity
             | Win Entity
             | Lose Entity
             | Lurk Entity
             | RunAway Entity
  deriving (Eq, Show)

data EventReport = Report ActionEvent [Outcome]

{-
  example = Report (Evt Melee shoggoth player Nothing Nothing) -- tentacles
                   [TakeDamage 4000, Die player]
-}

subj, obj :: Nominable a => a -> String
subj = nominative . name
obj = accusative . name

cverb :: Nominable a => a -> String -> String
cverb a = conj a . verb

aeverb :: Nominable a => a -> Action -> String
aeverb a = cverb a . downcase . show

-- TODO Rewrite all of this to use randomness, vocabulary, etc.
instance Effable Outcome where
  describe (TakeDamage a v) =
    unwords [subj a, cverb a "take", show v, "damage"]
  describe (Die a)
    | ai a == Player = "you perish"
    | otherwise = unwords [subj a, cverb a "collapse", "in a pool of blood"]
  describe (NearDeath a)
    | ai a == Player = if hp a > 0
                          then "you feel woozy from loss of blood"
                          else "your hit points dwindle to zero"
    | otherwise = unwords [subj a, cverb a "stagger", "under the blow"]
  describe (Win a) = unwords [subj a, cverb a "emerge", "victorious"]
  describe (Lose a) = unwords [subj a, cverb a "be", "defeated"]
  describe (Lurk a) = unwords [describe a, cverb a "be",
    "lurking in the darkness"]
  describe (RunAway a) = unwords [subj a, cverb a "escape", "with",
    genitive (name a), "life.."]
  -- describe ev = show ev

instance Effable ActionEvent where
  describe (VI Goto a) = "you climb back up the well"
  describe (VI v a) = unwords [subj a, aeverb a v]
  describe (VT v a p) = unwords [subj a, aeverb a v, obj p]
  describe _ = "UNIMPLEMENTED"

instance Effable EventReport where
  describe (Report (VT Attack a p) outcomes)
    | nodamage = unwords [subj a, theverb, obj p, "to no effect"]
    | wasFatal = unwords [subj a, "mortally", theverb, obj p, amt]
    | otherwise = unwords [subj a, theverb, obj p, amt]
    where amt = unwords ["for", show damage, "damage"]
          damage = let Just (TakeDamage _ v) = find damageTest outcomes in v
          theverb = conj a (verb "strike")
          nodamage = not $ any damageTest outcomes
          damageTest (TakeDamage _ _) = True
          damageTest _ = False
          wasFatal = any fatalTest outcomes
          fatalTest (Die _) = True
          fatalTest _ = False
  describe (Report v outcomes) = unlines (describe v : map describe outcomes)
