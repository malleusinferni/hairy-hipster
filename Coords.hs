module Coords where

import Data.Vec.Packed

type Coords = Vec3I

data Location = OnMap Coords
  deriving (Eq, Show)

data Cardinal = North | Northeast
              | East  | Southeast
              | South | Southwest
              | West  | Northwest
              | Up | Down
  deriving (Eq, Show, Ord, Enum)

zyx :: Int -> Int -> Int -> Coords
zyx z y x = Vec3I x y z

xy (Vec3I x y _) = Vec2I x y

dirToCoords :: Cardinal -> Coords
dirToCoords dir = Vec3I x y z
  where x | dir `elem` [Northeast, East, Southwest] = 1
          | dir `elem` [Northwest, West, Southwest] = -1
          | otherwise = 0
        y | dir `elem` [Northwest, North, Northeast] = 1
          | dir `elem` [Southwest, South, Southeast] = -1
          | otherwise = 0
        z | dir == Up = 1
          | dir == Down = -1
          | otherwise = 0

radials :: [Coords]
radials = map dirToCoords [North .. Northwest]

neighbors :: Coords -> [Coords]
neighbors o = map (o +) radials
