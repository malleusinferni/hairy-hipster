{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Control.Monad (replicateM_, when, liftM2)
import Data.List (sortBy)

import AI
import UI
import Describe
import Grammar.Atom

import Support.Coords
import Support.Rand

import World
import World.Core
import World.Entity

import Entity.Core
import Entity.Material
import Entity.Body
import Entity.Species
import Entity.Value as V
import qualified Entity.Trait as K

import Event.Action

import World.Location

playerSurvives :: Game Bool
playerSurvives = do
  players <- getEntitiesWhere (liftM2 (&&) isAlive isPlayer)
  return $ not (null players)

playTurn :: Game ()
playTurn = do
  [player] <- getEntitiesWhere isPlayer
  survivors <- getEntitiesWhere isAlive
  localEntities <- getEntitiesNear player
  let fastEntities = sortBy faster localEntities
      faster x y = speed y `compare` speed x
  if aboveGround (location player)
     then say $ if length survivors > 1
                   then "You escape with your life..."
                   else "You emerge victorious."
     else doTick (map eid fastEntities) playTurn

doTick :: [EID] -> Game () -> Game ()
doTick [] z = z
doTick (x : xs) z = do
  announce =<< tick x
  check <- playerSurvives
  when check $ doTick xs z

makePlayer :: Game EID
makePlayer = do
  (playerSpecies:_) <- asks speciesData
  makeMob playerSpecies True

makeEnemy :: Game EID
makeEnemy = do
  anySpecies <- randomSpecies
  makeMob anySpecies False

makeBody :: Species -> Game Body
makeBody species = do
  size <- anyIn $ sizeRangeFor species
  let material = Flesh
  return Body{..}

makeMob :: Species -> Bool -> Game EID
makeMob species isPlayer = do
  eid <- nextEID
  body <- makeBody species
  aRoom <- onGrid `fmap` anyRoom
  let traits = K.make [ K.Species ~> species
                      , K.IsPlayer ~> isPlayer
                      , K.Strength ~> strengthFor body
                      , K.HitPoints ~> maxHPFor body
                      , K.Dexterity ~> dexterityFor body species
                      , K.Location ~> if isPlayer
                                         then zyx 0 0 0
                                         else aRoom
                      ]
  eid `putAI` makeAI isPlayer eid
  storeEntity Entity{..}
  return eid

newGame :: IO ()
newGame = makeWorld >>= runReaderT playGame

playGame :: Game ()
playGame = do
  say "You climb down the well."
  _playerID <- makePlayer
  numEnemies <- fuzz 5
  replicateM_ numEnemies makeEnemy
  playTurn

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  when retry main
