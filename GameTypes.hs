{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GameTypes
  ( EID
  , Species(..)
  , Material(..)
  , Body(..)
  , Entity(..)
  , AI(..)
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
  , TrigMap
  ) where

import Data.IORef
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader

import Entity.Core
import Entity.Species
import Entity.Material
import Entity.Body

import AI.Trigger
import AI.Action
import AI.Event

import Coords
import Describe

-- An object in the game, usually with a physical body
data Entity = Entity
  { eid :: EID
  , ai :: AI
  , hp :: Int
  , body :: Body
  , isPlayer :: Bool
  , location :: Coords
  , species :: Species
  , power :: Int
  } deriving (Show)

instance Eq Entity where
  Entity { eid = lhs } == Entity { eid = rhs } = lhs == rhs

instance Ord Entity where
  compare (Entity { eid = lhs }) (Entity { eid = rhs }) = compare lhs rhs

instance Nominable Entity where
  name a | isPlayer a = noun You
  name (Entity { species = s })= noun (The s)

instance Effable Entity where
  describe e = nominative $ noun subj
    where subj = An $ Adj howtall whatspecies
          howtall = unwords [numWord $ inFeet, "foot tall"]
          whatspecies = species e
          inFeet = round (toRational s / 12)
          s = size (body e)

data AI = AI
  { methods :: TrigMap
  , ifMissing :: Responder
  , super :: Maybe AI
  } deriving (Show)

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
  , speciesData :: [Species]
  , locations :: LevelMap
  }

type Game a = ReaderT World IO a

type LevelMap = (M.Map Coords Room, M.Map Coords [Corridor])

type Selector a = World -> IORef a

type Responder = Trigger -> Game Action

instance Show Responder where
  show _ = "<responder function>"

type TrigMap = HM.HashMap Trigger Responder

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
