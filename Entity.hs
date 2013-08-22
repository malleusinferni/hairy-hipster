module Entity where

data Entity = Entity {
    eid :: ID,
    ai :: AI,
    hp :: Int,
    species :: Species,
    power :: Int,
    name :: String
  } deriving (Show)

instance Eq Entity where
  Entity { eid = lhs } == Entity { eid = rhs } = lhs == rhs

instance Ord Entity where
  compare (Entity { eid = lhs }) (Entity { eid = rhs }) = compare lhs rhs

data Species = Shoggoth | Goblin | Merovingian
  deriving (Eq, Show, Ord, Enum, Bounded)

newtype ID = EID Int
  deriving (Eq, Show, Ord)

data AI = Player | Monster
  deriving (Eq, Show, Ord)

hpRangeFor Goblin = (5, 25)
hpRangeFor Merovingian = (15, 35)
hpRangeFor Shoggoth = (25, 45)

strRangeFor Shoggoth = (10, 20)
strRangeFor _ = (5, 15)
