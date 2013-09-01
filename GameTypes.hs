{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GameTypes where

import Data.IORef
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad.Reader

import Coords
import Table

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

-- Entities are indexed by their unique IDs
type EID = Int

instance Eq Entity where
  Entity { eid = lhs } == Entity { eid = rhs } = lhs == rhs

instance Ord Entity where
  compare (Entity { eid = lhs }) (Entity { eid = rhs }) = compare lhs rhs

data Species = Species
  { speciesName :: String
  , minHeight :: Int
  , maxHeight :: Int
  } deriving (Eq, Show, Ord)

instance Tabular Species where
  readRecord = do
    speciesName <- copyField "Name"
    minHeight <- readField "Min height"
    maxHeight <- readField "Max height"
    return Species{..}

data AI = AI
  { methods :: TrigMap
  , ifMissing :: Responder
  , super :: Maybe AI
  } deriving (Show)

-- Commands which an actor AI may issue in response to Tick
data Action = Attack -- Damage another entity
            | Eat -- Consume an entity (dead or living!)
            | Go Cardinal -- Actor goes to a different location
            | Take | Put -- Move stuff between world and inventory
            | Open | Close -- Door, chest, portal...?
            | Ask | Tell -- Includes reading and writing
            | Copulate -- Laying eggs???
            | Rest -- Remain still and recuperate
            | DoNothing -- Really do nothing
            | Look -- NOTE: Looking around doesn't consume a turn!
  deriving (Eq, Show)

-- Prompts to which an AI may (or may not) respond
data Trigger = Tick
             | Impacted Int
             | Pierced Int
             | Slashed Int
             | Burned Int
             | Seen
  deriving (Eq, Show)

-- Descriptive account of one result of an action
data Event = Outcome :& [EvArg]

-- Effect of the action on the patient (IMPORTANT!)
data Outcome = NothingHappens
             | TakeDamage
             | NearDeath
             | Heal
             | Stand
             | Walk
             | See
             | Die
             | Win
             | Lose
             | Fail
  deriving (Eq, Show)

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

type TrigMap = IM.IntMap Responder

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

data Body = Body
  { material :: Material
  , size :: Int
  } deriving (Eq, Show)

data Material = Flesh
              | Steel
              | Carapace
              | Air
              | Grass
              | Stone
              | Masonry
              | Dirt
              | Sand
              | Blood
              | Sandstone
              | Wood
  deriving (Eq, Show, Ord, Enum)
