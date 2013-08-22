import Control.Monad (replicateM_)

import World
import Entity
import Room
import AI
import UI
import Rand
import Action

playerAmong = elem Player . map ai

playerSurvives = playerAmong `fmap` getEntitiesWhere stillAlive

playTurn :: Game ()
playTurn = getEntitiesWhere stillAlive >>= go
  where go [] = say "None survive..."
        go [entity] = tellVictory entity
        go combatants = playseq $ map tick combatants

playseq [] = playTurn
playseq (action : actions) = do
  test <- playerSurvives
  if test
     then do action; playseq actions
     else return ()

makePlayer :: Game Entity
makePlayer = makeMob Merovingian Player

makeEnemy :: Game Entity
makeEnemy = do
  species <- randomSpecies
  makeMob species Monster

makeMob :: Species -> AI -> Game Entity
makeMob species ai = do
  eid <- nextID
  hp <- anyIn (hpRangeFor species)
  str <- anyIn (strRangeFor species)
  let name | ai == Player = "player"
           | otherwise = show species
  return $ Entity { eid = eid, hp = hp, power = str,
        ai = ai, species = species }

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
    announce (Lurk enemy)
    storeEntity enemy
  say "You bare your sword and leap into the fray."
  storeEntity playerEntity
  playTurn

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  if retry then main else return ()
