{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module GameTypes
  ( AI
  , World(..)
  , Game
  , Responder
  , Selector
  ) where

import Data.IORef
import qualified Data.IntMap.Strict as IM
import Control.Monad.Reader

import Entity.Core
import Entity.Species

import AI.Trigger
import AI.Action
import AI.Binding

import World.Location

type AI = Bind Trigger (Game Action)

-- Master game state record
data World = World
  { getEID :: IO EID
  , entities :: IORef [Entity]
  , bindings :: IORef (IM.IntMap AI)
  , speciesData :: [Species]
  , locations :: LevelMap
  }

type Game a = ReaderT World IO a

type Selector a = World -> IORef a

type Responder = Trigger -> Game Action
