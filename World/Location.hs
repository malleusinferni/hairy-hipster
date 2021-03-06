{-# LANGUAGE BangPatterns #-}
module World.Location where

import qualified Data.Map as M
import Data.Text (pack)

import Describe
import Grammar.Atom

import Support.Coords

import Entity.Material

type LevelMap = (M.Map Coords Room, M.Map Coords [Corridor])

data Room = Room
  { onGrid :: !Coords
  , roomName :: !String
  , description :: [Leaf]
  , walls :: !Material
  , floors :: !Material
  } deriving (Eq, Show)

data Corridor = Corridor
  { endpoints :: !(Coords, Coords)
  , doorName :: !String
  } deriving (Eq, Show)

instance Effable Corridor where
  describe c = the [Word . pack $ doorName c]
