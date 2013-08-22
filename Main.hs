import System.IO (stdout, hFlush)
import System.Random (randomRIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (replicateM)
import Control.Monad.Reader
import Data.IORef

import Entity
import Room

data World = World {
    getID :: IO ID,
    entities :: IORef [Entity],
    locations :: [Room]
  }

type Game a = ReaderT World IO a

say = liftIO . putStrLn
saywords = say . unwords

stillAlive = (> 0) . hp

playerAmong = any (== Player) . map ai

playTurn :: Game ()
playTurn = asks entities >>= liftIO . readIORef >>= go
  where go [] = say "None survive..."
        go [Entity { ai = Player }] = say "You emerge victorious!"
        go (attacker : defender : bystanders) = do
          defender' <- dealDamage attacker defender
          tellHealth defender'
          let rotated = concat [[defender'], bystanders, [attacker]]
              survivors = filter stillAlive rotated
          entities $= survivors
          if playerAmong survivors
             then playTurn
             else saywords ["The", name attacker, "has defeated you..."]

dealDamage attacker defender = do
  let p = power attacker
  amount <- liftIO $ randomRIO (p, p + 3)
  saywords ["The", name attacker, "hits the", name defender,
    "for", show amount, "damage!"]
  return defender { hp = hp defender - amount }

tellHealth :: Entity -> Game ()
tellHealth (Entity { hp = hp, ai = Player })
  | hp > 10 = say "You feel fine."
  | hp > 0 = say "You feel woozy from blood loss."
  | otherwise = say "Your hit points dwindle to zero. You perish!"
tellHealth npc@(Entity { hp = hp })
  | hp <= 0 = saywords ["The", name npc, "collapses in a pool of blood."]
tellHealth _ = return ()

streamIDs :: Int -> IO (IO ID)
streamIDs n = do
  ref <- newEmptyMVar
  forkIO (mapM_ (putMVar ref) [n ..])
  return (fmap EID $ takeMVar ref)

nextID :: Game ID
nextID = liftIO =<< asks getID

makeWorld :: IO World
makeWorld = do
  stream <- streamIDs 0
  ref <- newIORef []
  let well = Door { doortype = "well", doordir = Down }
      outside = Room { placename = "outside", doors = [(well, inside)] }
      inside = Room { placename = "inside", doors = [] }
  return World { getID = stream, entities = ref,
    locations = [outside, inside] }

(%=) :: (World -> IORef a) -> (a -> a) -> Game ()
sel %= action = do
  ref <- asks sel
  liftIO $ modifyIORef' ref action

($=) :: (World -> IORef a) -> a -> Game ()
sel $= value = do
  ref <- asks sel
  liftIO $ writeIORef ref value

makePlayer :: Game Entity
makePlayer = do
  eid <- nextID
  return Entity {
      eid = eid,
      ai = Player,
      species = Merovingian,
      power = 8,
      hp = 30,
      name = "player"
    }

makeEnemy :: Game Entity
makeEnemy = do
  eid <- nextID
  enemy <- liftIO $ do
    species <- randomSpecies
    hp <- randomRIO (hpRangeFor species)
    str <- randomRIO (strRangeFor species)
    return $ Entity { eid = eid, hp = hp, power = str,
          ai = Monster, name = show species, species = species }
  entities %= (enemy :)
  return enemy

randomSpecies :: IO Species
randomSpecies = toEnum `fmap` randomRIO (low, high)
  where [low, high] = map fromEnum range
        range = [minBound, maxBound] :: [Species]

newGame :: IO ()
newGame = makeWorld >>= runReaderT playGame

playGame :: Game ()
playGame = do
  say "You climb down the well."
  playerEntity <- makePlayer
  numEnemies <- liftIO (randomRIO (1, 5))
  enemies <- replicateM numEnemies $ do
    enemy <- makeEnemy
    saywords ["A", name enemy, "with", show (hp enemy),
      "HP is lurking in the darkness."]
    return enemy
  say "You bare your sword and leap into the fray."
  entities %= (playerEntity :)
  playTurn

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

main :: IO ()
main = do
  newGame
  response <- prompt "Play again? [yn] "
  case response of
    [] -> main
    ('y':_) -> main
    ('Y':_) -> main
    _ -> return ()
