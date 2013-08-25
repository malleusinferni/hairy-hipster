module Room where

import Coords

data Room = Room {
    onGrid :: Coords,
    exits :: [(Cardinal, Room)]
  }
  deriving (Eq, Show)

data Cardinal = North | East | South | West | Up | Down
  deriving (Eq, Show, Ord, Enum)
