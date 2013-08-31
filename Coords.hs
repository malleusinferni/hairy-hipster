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

zyx :: Int -> Int -> Int -> Vec3I
zyx z y x = Vec3I x y z

dirToCoords :: Cardinal -> Vec3I
dirToCoords North = Vec3I 0 1 0
dirToCoords East  = Vec3I 1 0 0
dirToCoords South = Vec3I 0 (-1) 0
dirToCoords West  = Vec3I (-1) 0 0

dirToCoords Northeast = Vec3I 1 1 0
dirToCoords Southeast = Vec3I 1 (-1) 0
dirToCoords Southwest = Vec3I (-1) (-1) 0
dirToCoords Northwest = Vec3I (-1) 1 0

dirToCoords Up = Vec3I 0 0 1
dirToCoords Down = Vec3I 0 0 (-1)

radials :: [Vec3I]
radials = map dirToCoords [North .. Northwest]

neighbors :: Vec3I -> [Vec3I]
neighbors o = map (o +) radials

showCoord :: Vec3I -> String
showCoord (Vec3I x y z) = show (x, y, z)

printCoord :: Vec3I -> IO ()
printCoord = putStrLn . showCoord

printCoords :: [Vec3I] -> IO ()
printCoords = mapM_ printCoord
