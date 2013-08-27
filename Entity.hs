module Entity where

import GameTypes
import Describe
import Coords

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

hpRangeFor Goblin = (5, 25)
hpRangeFor Merovingian = (15, 35)
hpRangeFor Shoggoth = (25, 45)
hpRangeFor _ = (10, 30)

strRangeFor Shoggoth = (10, 20)
strRangeFor _ = (5, 15)

isActor = (`elem` [Player, Actor]) . aiType . ai

stillAlive = (> 0) . hp
