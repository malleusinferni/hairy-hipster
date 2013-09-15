module World.Location where

import qualified Data.Map as M

import Support.Coords

import Entity.Material

type LevelMap = (M.Map Coords Room, M.Map Coords [Corridor])

data Room = Room
  { onGrid :: Coords
  , roomName :: String
  , description :: String
  , walls :: Material
  , floors :: Material
  } deriving (Eq, Show)

data Corridor = Corridor
  { endpoints :: (Coords, Coords)
  , doorName :: String
  } deriving (Eq, Show)
