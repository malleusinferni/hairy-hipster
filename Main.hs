{-# LANGUAGE RecordWildCards #-}
import Control.Monad (replicateM_, when, liftM2)
import Data.Function (on)
import Data.List (sortBy)

import AI
import UI
import Describe

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
  let hp = maxHPFor body
      power = strengthFor body
      speed = dexterityFor body species
      location
        | isPlayer = zyx 0 0 0
        | otherwise = aRoom
      traits = K.make [ K.Location ~> location
                      , K.Strength ~> power
                      , K.Dexterity ~> speed
                      , K.HitPoints ~> hp
                      , K.Species ~> species
                      , K.IsPlayer ~> isPlayer
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
