module Entity where

data Entity = Entity {
    eid :: ID,
    hp :: Int,
    species :: Species,
    power :: Int,
    name :: String
  } deriving (Eq, Ord, Show)

data Species = Shoggoth | Goblin | Merovingian
  deriving (Eq, Show, Ord, Enum, Bounded)

data ID = Player | EID Int deriving (Eq, Ord, Show)
