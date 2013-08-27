module Entity where

import qualified Data.IntMap as IM

import GameTypes
import Describe
import Coords

instance Nominable Species where
  name s = Noun (describe s) (describe s ++ "'s") (describe s) False

instance Effable Species where
  describe Merovingian = "Merovingian"
  describe s = downcase (show s)

instance Nominable Entity where
  name a | isPlayer a = noun You
  name (Entity { species = s })= noun (The s)

instance Effable Entity where
  describe e = unwords [subj, "with", show (hp e), "HP"]
    where subj = nominative . noun . An $ species e

-- TODO Find a less obnoxious way to accomplish this
triggerCode :: Trigger -> Int
triggerCode (Tick) = 0
triggerCode (Impacted _) = 1
triggerCode (Pierced _) = 2
triggerCode (Slashed _) = 3
triggerCode (Burned _) = 4
triggerCode (Seen) = 5

getResponder :: Trigger -> Entity -> Maybe Responder
getResponder t = IM.lookup (triggerCode t) . hooks . ai

respondsTo :: Trigger -> Entity -> Bool
respondsTo t entity =
  case getResponder t entity of
    Just _ -> True
    Nothing -> False

removeHooks :: [Trigger] -> AI -> AI
removeHooks triggers oldAI = oldAI { hooks = newHooks }
  where newHooks = foldr prune oldHooks triggers
        prune = IM.delete . triggerCode
        oldHooks = hooks oldAI

makeRespMap r = IM.fromList [(triggerCode Tick, r)]

hpRangeFor Goblin = (5, 25)
hpRangeFor Merovingian = (15, 35)
hpRangeFor Shoggoth = (25, 45)
hpRangeFor _ = (10, 30)

strRangeFor Shoggoth = (10, 20)
strRangeFor _ = (5, 15)

isActor = respondsTo (Tick)

stillAlive = (> 0) . hp
