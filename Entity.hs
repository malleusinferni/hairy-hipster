{-# LANGUAGE RecordWildCards #-}
module Entity where

import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM

import GameTypes
import Describe

instance Nominable Species where
  name s = Noun (describe s) (describe s ++ "'s") (describe s) False

instance Effable Species where
  describe = speciesName

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

makeMethodMap :: [(Int, Responder)] -> TrigMap
makeMethodMap = IM.fromList

-- Size in inches
sizeRangeFor :: Species -> (Int, Int)
sizeRangeFor species = (minHeight species, maxHeight species)

inFeet :: Entity -> Int
inFeet e = size (body e) `rdiv` 12

rdiv :: Int -> Int -> Int
rdiv q d = round $ toRational q / toRational d

maxHPFor, strengthFor :: Body -> Int
maxHPFor body = size body `rdiv` 3
strengthFor body = size body `rdiv` 5

isNearDeath, isDead, isAlive :: Entity -> Bool
isNearDeath e = hp e <= maxHPFor (body e) `rdiv` 5
isAlive = (> 0) . hp
isDead = not . isAlive

isOpponentOf :: Entity -> Entity -> Bool
isOpponentOf self e =
  isAlive e &&
    eid e /= eid self &&
      location e == location self
