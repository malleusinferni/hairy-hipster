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

xy :: Coords -> Vec2I
xy (Vec3I x y _) = Vec2I x y

quadrant :: Coords -> Cardinal
quadrant (Vec3I x y _)
  | x == 0 && y > 0 = North
  | x == 0 && y < 0 = South
  | y == 0 && x > 0 = East
  | y == 0 && x < 0 = West
  | x > 0 && y > 0 = Northeast
  | x > 0 && y < 0 = Southeast
  | x < 0 && y > 0 = Northwest
  | x < 0 && y < 0 = Southwest

quantize :: Coords -> Coords
quantize (Vec3I x y _)
  | abs x > 2 * abs y = use x 0
  | abs y > 2 * abs x = use 0 y
  | otherwise = use x y
  where use x' y' = Vec3I (signum x') (signum y') 0

dirToCoords :: Cardinal -> Coords
dirToCoords dir = Vec3I x y z
  where x | dir `elem` [Northeast, East, Southeast] = 1
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
