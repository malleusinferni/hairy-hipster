module Entity.Body where

import Entity.Species
import Entity.Material

import Support.Measure

data Body = Body
  { material :: Material
  , size :: Int
  } deriving (Eq, Show)

maxHPFor, strengthFor :: Body -> Int
maxHPFor body = size body `rdiv` 3
strengthFor body = size body `rdiv` 5

dexterityFor :: Body -> Species -> Int
dexterityFor body species = lerp (size body) low high
  where (xl, xh) = sizeRangeFor species
        low = (xl, 25)
        high = (xh, 5)
