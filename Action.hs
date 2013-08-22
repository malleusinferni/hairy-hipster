module Action where

import Describe
import Entity

data Action = Melee | Ranged -- Damage another entity
            | Eat -- Consume an entity (dead or living!)
            | Goto -- Actor goes to a different location
            | Take | Put -- Move stuff between world and inventory
            | Open | Close -- Door, chest, portal...?
            | Ask | Tell -- Includes reading and writing
            | Copulate -- Laying eggs???
            | Rest -- Do nothing (recuperate if possible)
            -- NOTE: Looking around doesn't consume a turn!

data ActionEvent = Evt {
    verbType :: Action,
    agent :: Entity,
    patient :: Maybe Entity,
    theme :: Maybe Entity,
    instrument :: Maybe Entity
  }

data Outcome = Damage {
    subject :: Entity,
    method :: Verb,
    defender :: Entity,
    amount :: Int
  } | Perish {
    subject :: Entity
  } | Win {
    subject :: Entity
  } | Lurk {
    subject :: Entity
  } deriving (Eq, Show)

data EventReport = Report ActionEvent [Outcome]

{-
  example = Report (Evt Melee shoggoth player Nothing Nothing) -- tentacles
                   [TakeDamage 4000, Die player]
-}

-- TODO Rewrite all of this to use randomness, vocabulary, etc.
instance Effable Outcome where
  describe (Damage s m d a)
    | hp d > 0 = unwords [subj, averb, obj, amt]
    | otherwise = unwords [subj, "mortally", conj s (verb "wound"), obj]
    where subj = nominative (name s)
          obj = accusative (name d)
          amt = unwords ["for", show a, "damage"]
          averb = conj s m
  describe (Perish s) = unwords [nominative (name s), conj s (verb "perish")]
  describe (Win s) = unwords [nominative (name s), conj s (verb "emerge"),
    "victorious"]
  describe (Lurk s) = unwords [describe s, conj s (verb "be"), "lurking",
    "in the darkness"]
