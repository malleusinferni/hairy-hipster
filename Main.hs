{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_, when)

import GameTypes
import World
import Entity
import AI
import UI
import Rand
import Action
import Describe

playerAmong :: [Entity] -> Bool
playerAmong = any isPlayer

playerSurvives :: Game Bool
playerSurvives = playerAmong `fmap` getEntitiesWhere isAlive

playTurn :: Game ()
playTurn = getEntitiesWhere isAlive >>= go
  where go [] = say "None survive..."
        go [entity] = tellVictory entity
        go combatants = doTick combatants playTurn

doTick [] z = z
doTick (x : xs) z = do
  report <- tick x
  say $ describe report
  check <- playerSurvives
  when check $ doTick xs z

makePlayer :: Game Entity
makePlayer = makeMob Merovingian True

makeEnemy :: Game Entity
makeEnemy = do
  species <- randomSpecies
  makeMob species False

makeBody :: Species -> Game Body
makeBody species = do
  size <- anyIn $ sizeRangeFor species
  let material = Flesh
  return Body{..}

makeMob :: Species -> Bool -> Game Entity
makeMob species isPlayer = do
  eid <- nextEID
  body <- makeBody species
  location <- anyRoom
  let ai = makeAI isPlayer eid
      hp = maxHPFor body
      power = strengthFor body
  return Entity{..}

makeAI :: Bool -> EID -> AI
makeAI True entity = playerAI entity
makeAI False entity = actorAI entity

randomSpecies :: Game Species
randomSpecies = toEnum `fmap` anyIn (low, high)
  where [low, high] = map fromEnum range
        range = [minBound, maxBound] :: [Species]

newGame :: IO ()
newGame = makeWorld >>= runReaderT playGame

playGame :: Game ()
playGame = do
  say "You climb down the well."
  playerEntity <- makePlayer
  numEnemies <- anyIn (1, 5)
  replicateM_ numEnemies $ do
    enemy <- makeEnemy
    announce (See :& [Agent playerEntity, Patient enemy])
    storeEntity enemy
  say "You bare your sword and leap into the fray."
  storeEntity playerEntity
  playTurn

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  when retry main
