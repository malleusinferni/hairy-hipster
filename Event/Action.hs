{-# LANGUAGE OverloadedStrings #-}
module Event.Action where

import Describe
import Grammar.Atom

import Support.Coords

import AI.Trigger
import AI.Event
import AI.Action

import Entity.Core

import World.Location

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

-- TODO Rewrite all of this to use randomness, vocabulary, etc.
instance Effable Event where
  describe (Walk :& (Agent a : WhichWay Up : _)) =
    describe a ++ "climbs up the well"
  describe (TakeDamage :& (Agent a : Patient p : ByAmount i : _)) =
    describe a ++ (word "strikes" : describe p) ++
      map word ["for", show i, "damage"]
  describe (NearDeath :& Patient p : _)
    | isPlayer p = (describe p ++ "feels woozy from blood loss")
    | otherwise = (describe p ++ "staggers under the blow")
  describe (See :& Agent _ : Patient p : _) =
    describe p ++ "is lurking in the darkness"
  describe (See :& Agent _ : Via d : OutOf _ : WhichWay c : _) =
    describe d ++ (word "leads" : describe c)
  describe (Die :& Agent a : Patient p : _) =
    describe a ++ (word "defeats" : describe p)
  describe (Die :& Patient p : _) =
    describe p ++ "perishes"
  describe (Win :& Patient p : _) =
    describe p ++ "emerges victorious"
  describe (Lose :& Patient p : _) =
    describe p ++ "escapes with its life"
  describe (Fail :& Tried (Go d) : Agent a : _) =
    describe a ++ "can't go" ++ describe d
  describe (Fail :& Tried v : _) =
    "there's nothing to" ++ [word (show v)]
  describe (Heal :& Patient p : _) =
    describe p ++ "recovers a little health"
  describe (Walk :& Patient p : WhichWay u : Via d : _)
    | u `elem` [Up, Down] =
      concat [describe p, "climbs", describe u, describe d]
    | otherwise =
      describe p ++ "goes through" ++ describe d
  describe (Walk :& Patient _ : Into d : _) = [word (description d)]
  describe (v :& Agent a : Patient p : _) =
    concat [describe a, [word (show v)], describe p]
  describe (v :& Agent a : _ ) = describe a ++ [word (show v)]
  describe (v :& Patient p : _) = describe p ++ [word (show v)]
  describe (_ :& []) = "nothing happens"
  describe _ = "UNIMPLEMENTED"

report :: EventReport -> [Leaf]
report (_ :=> [cause, NothingHappens :& (Agent _ : Patient _ : _)]) =
  sentence (describe cause ++ "to no effect")
report (_ :=> outcomes) = paragraph outcomes
