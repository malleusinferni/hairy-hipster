module Coords where

data Coords = ZYX Int Int Int
  deriving (Eq, Show)

data Location = OnMap Coords
  deriving (Eq, Show)
