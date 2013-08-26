module Room where

import Coords

data Room = Room {
    rid :: RID,
    onGrid :: Coords,
    exits :: [(Cardinal, RID)]
  }
  deriving (Eq, Show)

data Cardinal = North | East | South | West | Up | Down
  deriving (Eq, Show, Ord, Enum)

type RID = Int
