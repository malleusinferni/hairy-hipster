import System.Random (randomRIO)
import Control.Monad (replicateM_)

import World
import Entity
import Room
import AI
import UI

playerAmong = any (== Player) . map ai

playTurn :: Game ()
playTurn = getEntities >>= go
  where go [] = say "None survive..."
        go [Entity { ai = Player }] = say "You emerge victorious!"
        go (attacker : combatants) = do
          survivors <- runAI attacker combatants
          entities $= survivors
          if playerAmong survivors
             then playTurn
             else saywords ["The", name attacker, "has defeated you..."]

makePlayer :: Game Entity
makePlayer = makeMob Merovingian Player

makeEnemy :: Game Entity
makeEnemy = do
  species <- randomSpecies
  makeMob species Monster

makeMob :: Species -> AI -> Game Entity
makeMob species ai = do
  eid <- nextID
  hp <- liftIO $ randomRIO (hpRangeFor species)
  str <- liftIO $ randomRIO (strRangeFor species)
  let name | ai == Player = "player"
           | otherwise = show species
  return $ Entity { eid = eid, hp = hp, power = str,
        ai = ai, name = name, species = species }

randomSpecies :: Game Species
randomSpecies = liftIO $ toEnum `fmap` randomRIO (low, high)
  where [low, high] = map fromEnum range
        range = [minBound, maxBound] :: [Species]

newGame :: IO ()
newGame = makeWorld >>= runReaderT playGame

playGame :: Game ()
playGame = do
  say "You climb down the well."
  playerEntity <- makePlayer
  numEnemies <- liftIO (randomRIO (1, 5))
  replicateM_ numEnemies $ do
    enemy <- makeEnemy
    saywords ["A", name enemy, "with", show (hp enemy),
      "HP is lurking in the darkness."]
    storeEntity enemy
  say "You bare your sword and leap into the fray."
  storeEntity playerEntity
  playTurn

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  if retry then main else return ()
