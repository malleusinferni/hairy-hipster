module Room where

import Coords

data Room = Room {
    rid :: RID,
    onGrid :: Coords,
    exits :: [(Cardinal, RID)]
  }
  deriving (Eq, Show)

type RID = Int
