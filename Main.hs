{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_)

import GameTypes
import World
import Entity
import AI
import UI
import Rand
import Action

playerAmong :: [Entity] -> Bool
playerAmong = any isPlayer

playerSurvives :: Game Bool
playerSurvives = return True

playTurn :: Game ()
playTurn = getEntities >>= go
  where go [] = say "None survive..."
        go [entity] = tellVictory entity
        go combatants = while playerSurvives (map tick combatants) playTurn

while :: (Monad m) => m Bool -> [m ()] -> m () -> m ()
while c [] z = z
while c (f : fs) z = do
  test <- c
  if test
     then f >> while c fs z
     else return ()

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
  if retry then main else return ()
