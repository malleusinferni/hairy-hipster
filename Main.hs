{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_)

import World
import Entity
import Room
import AI
import UI
import Rand
import Action

playerAmong = any isPlayer

playerSurvives = playerAmong `fmap` getEntitiesWhere isActor

playTurn :: Game ()
playTurn = getEntitiesWhere isActor >>= go
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
makePlayer = makeMob Merovingian Player

makeEnemy :: Game Entity
makeEnemy = do
  species <- randomSpecies
  makeMob species Actor

makeMob :: Species -> AIType -> Game Entity
makeMob species aitype = do
  eid <- nextEID
  hp <- anyIn (hpRangeFor species)
  power <- anyIn (strRangeFor species)
  location <- anyRoom
  let ai = makeAI aitype eid
  return Entity{..}

makeAI aiType entity = AI{..}

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
