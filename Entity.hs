{-# LANGUAGE RecordWildCards #-}
module Entity where

import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM

import GameTypes
import Describe

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

respondTo :: Entity -> Responder
respondTo Entity{ ai = ai } t = IM.findWithDefault ifMissing t' methods t
  where AI{..} = ai
        t' = triggerCode t

popAI :: AI -> AI
popAI ai = ai `fromMaybe` super ai

makeRespMap :: Responder -> TrigMap
makeRespMap r = IM.fromList [(triggerCode Tick, r)]

makeMethodMap :: [(Int, Responder)] -> TrigMap
makeMethodMap = IM.fromList

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
numWord _ = undefined

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

isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= maxHPFor (body e) `rdiv` 5
isAlive = (> 0) . hp
isDead = not . isAlive
