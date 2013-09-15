module Entity.Body where

import Entity.Material

import Support.Measure

data Body = Body
  { material :: Material
  , size :: Int
  } deriving (Eq, Show)

maxHPFor, strengthFor :: Body -> Int
maxHPFor body = size body `rdiv` 3
strengthFor body = size body `rdiv` 5
