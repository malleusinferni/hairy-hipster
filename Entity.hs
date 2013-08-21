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

hpRangeFor Goblin = (5, 25)
hpRangeFor Merovingian = (15, 35)
hpRangeFor Shoggoth = (25, 45)

strRangeFor Shoggoth = (10, 20)
strRangeFor _ = (5, 15)
