module Action where

import Describe
import Entity

data Intent = FIGHT | ESCAPE
  deriving (Eq, Show, Read)

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
