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
  describe e = nominative $ noun subj
    where subj = An $ Adj howtall whatspecies
          howtall = unwords [numWord $ inFeet e, "foot tall"]
          whatspecies = species e

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

makeRespMap :: Responder -> TrigMap
makeRespMap r = IM.fromList [(triggerCode Tick, r)]

-- Size in inches
sizeRangeFor :: Species -> (Int, Int)
sizeRangeFor Goblin = (40, 55)
sizeRangeFor Merovingian = (60, 80)
sizeRangeFor Shoggoth = (50, 120)
sizeRangeFor Unseelie = (60, 100)

numWord 1 = "one"
numWord 2 = "two"
numWord 3 = "three"
numWord 4 = "four"
numWord 5 = "five"
numWord 6 = "six"
numWord 7 = "seven"
numWord 8 = "eight"
numWord 9 = "nine"
numWord 10 = "ten"
numWord 11 = "eleven"
numWord 12 = "twelve"
numWord 13 = "thirteen"
numWord 14 = "fourteen"
numWord 15 = "fifteen"
numWord 16 = "sixteen"
numWord 17 = "seventeen"
numWord 18 = "eighteen"
numWord 19 = "nineteen"
numWord 20 = "twenty"

inFeet :: Entity -> Int
inFeet e = size (body e) `rdiv` 12

rdiv :: Int -> Int -> Int
rdiv q d = round $ toRational q / toRational d

maxHPFor, strengthFor :: Body -> Int
maxHPFor body = size body `rdiv` 3
strengthFor body = size body `rdiv` 5

attackRangeFor :: Entity -> (Int, Int)
attackRangeFor e = (mid - err, mid + err)
  where mid = power e
        err = mid `rdiv` 10

isActor, isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= (maxHPFor $ body e) `rdiv` 5
isActor = respondsTo (Tick)
isDead = (<= 0) . hp
isAlive = not . isDead
