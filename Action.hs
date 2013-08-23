module Action where

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

data ActionEvent = Evt {
    verbType :: Action,
    agent :: Entity,
    patient :: Maybe Entity,
    theme :: Maybe Entity,
    instrument :: Maybe Entity
  }

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
    | ai a == Player = "your hit points dwindle to zero"
    | otherwise = unwords [subj a, cverb a "collapse", "in a pool of blood"]
  describe (NearDeath a)
    | ai a == Player = "you feel woozy from loss of blood"
    | otherwise = unwords [subj a, cverb a "be", "near death"]
  describe (Win a) = unwords [subj a, cverb a "emerge", "victorious"]
  describe (Lose a) = unwords [subj a, cverb a "be", "defeated"]
  describe (Lurk a) = unwords [describe a, cverb a "be",
    "lurking in the darkness"]
  describe (RunAway a) = unwords [subj a, cverb a "escape", "with",
    genitive (name a), "life.."]
  -- describe ev = show ev

instance Effable ActionEvent where
  describe (Evt v a (Just p) _ _) = unwords [subj a, aeverb a v, obj p]
  describe (Evt v a _ _ _) = unwords [subj a, aeverb a v]
  -- describe _ = "UNIMPLEMENTED"

instance Effable EventReport where
  describe (Report (Evt Attack a (Just p) _ _) outcomes)
    | wasFatal = unwords [subj, "mortally", theverb, obj, amt]
    | otherwise = unwords [subj, theverb, obj, amt]
    where subj = nominative (name a)
          obj = accusative (name p)
          amt = unwords ["for", show a, "damage"]
          theverb = conj a (verb "strike")
          wasFatal = any fatalTest outcomes
          fatalTest (Die e) = e == p
  describe (Report v outcomes) = unlines (describe v : map describe outcomes)
