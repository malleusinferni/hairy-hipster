{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_, when)

import GameTypes
import World
import Entity
import AI
import UI
import Rand
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

doTick :: [Entity] -> Game () -> Game ()
doTick [] z = z
doTick (x : xs) z = do
  report <- tick x
  say $ describe report
  check <- playerSurvives
  when check $ doTick xs z

makePlayer :: Game Entity
makePlayer = do
  (species:_) <- asks speciesData
  makeMob species True

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
  location <- onGrid `fmap` anyRoom
  let ai = makeAI isPlayer eid
      hp = maxHPFor body
      power = strengthFor body
  return Entity{..}

makeAI :: Bool -> EID -> AI
makeAI True entity = playerAI entity
makeAI False entity = actorAI entity

randomSpecies :: Game Species
randomSpecies = do
  species <- asks speciesData
  Just soSueMe <- anyOf species
  return soSueMe

newGame :: IO ()
newGame = makeWorld >>= runReaderT playGame

playGame :: Game ()
playGame = do
  say "You climb down the well."
  playerEntity <- makePlayer
  numEnemies <- fuzz 5
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
