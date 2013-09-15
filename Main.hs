{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_, when, liftM2)

import Coords
import GameTypes
import World
import Entity
import AI
import UI
import Rand
import Describe

import Entity.Material
import Entity.Body
import Entity.Species

playerSurvives :: Game Bool
playerSurvives = do
  players <- getEntitiesWhere (liftM2 (&&) isAlive isPlayer)
  return $ not (null players)

playTurn :: Game ()
playTurn = do
  [player] <- getEntitiesWhere isPlayer
  survivors <- getEntitiesWhere isAlive
  localEntities <- getEntitiesNear player
  if aboveGround (location player)
     then if length survivors > 1
             then say "You escape with your life..."
             else say "You emerge victorious."
     else doTick (map eid localEntities) playTurn

doTick :: [EID] -> Game () -> Game ()
doTick [] z = z
doTick (x : xs) z = do
  report <- tick x
  say $ describe report
  check <- playerSurvives
  when check $ doTick xs z

makePlayer :: Game EID
makePlayer = do
  (species:_) <- asks speciesData
  makeMob species True

makeEnemy :: Game EID
makeEnemy = do
  species <- randomSpecies
  makeMob species False

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
  let ai = makeAI isPlayer eid
      hp = maxHPFor body
      power = strengthFor body
      location
        | isPlayer = zyx 0 0 0
        | otherwise = aRoom
  storeEntity Entity{..}
  return eid

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
  _playerID <- makePlayer
  numEnemies <- fuzz 5
  replicateM_ numEnemies makeEnemy
  playTurn

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  when retry main
