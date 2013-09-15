{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module GameTypes
  ( AI
  , Event(..)
  , EventReport(..)
  , EvArg(..)
  , World(..)
  , Room(..)
  , Corridor(..)
  , Game
  , LevelMap
  , Responder
  , Selector
  ) where

import Data.IORef
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Control.Monad.Reader

import Entity.Core
import Entity.Species
import Entity.Material

import AI.Trigger
import AI.Action
import AI.Event
import AI.Binding

import Support.Coords

type AI = Bind Trigger (Game Action)

-- Descriptive account of one result of an action
data Event = Outcome :& [EvArg]

-- Semantic arguments to an Outcome
data EvArg = Agent Entity
           | Patient Entity
           | Using Entity
           | ByAmount Int
           | Into Room
           | OutOf Coords
           | WhichWay Cardinal
           | Via Corridor
           | Tried Action
  deriving (Eq, Show)

-- Bind cause to effect
data EventReport = Trigger :=> [Event]

infixr 1 :=>
infixr 2 :&

-- Master game state record
data World = World
  { getEID :: IO EID
  , entities :: IORef [Entity]
  , bindings :: IORef (IM.IntMap AI)
  , speciesData :: [Species]
  , locations :: LevelMap
  }

type Game a = ReaderT World IO a

type LevelMap = (M.Map Coords Room, M.Map Coords [Corridor])

type Selector a = World -> IORef a

type Responder = Trigger -> Game Action

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
