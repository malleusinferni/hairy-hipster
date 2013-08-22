import System.IO (stdout, hFlush)
import System.Random (randomRIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (replicateM_)
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
        go (attacker : combatants) = do
          survivors <- runAI attacker combatants
          entities $= survivors
          if playerAmong survivors
             then playTurn
             else saywords ["The", name attacker, "has defeated you..."]

runAI player@(Entity { ai = Player }) (defender : bystanders) = do
  yn <- liftIO . promptYN $ "Attack the " ++ name defender ++ "? [Yn] "
  if yn
     then do
       defender <- dealDamage player defender
       tellHealth defender
       return $ filter stillAlive ((defender : bystanders) ++ [player])
     else do
       say "Climbing back up the well, you escape with your life..."
       return [player]
runAI attacker (defender : bystanders) = do
  defender <- dealDamage attacker defender
  tellHealth defender
  return $ filter stillAlive ((defender : bystanders) ++ [attacker])
runAI attacker _ = do
  saywords ["The", name attacker, "emerges victorious!"]
  return [attacker]

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

type Selector a = World -> IORef a

(%=) :: Selector a -> (a -> a) -> Game ()
sel %= action = do
  ref <- asks sel
  liftIO $ modifyIORef' ref action

($=) :: Selector a -> a -> Game ()
sel $= value = do
  ref <- asks sel
  liftIO $ writeIORef ref value

storeEntity e = entities %= (e:)
updateEntity e = entities %= (map $ \i -> if e == i then e else i)

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
  liftIO $ do
    species <- randomSpecies
    hp <- randomRIO (hpRangeFor species)
    str <- randomRIO (strRangeFor species)
    return $ Entity { eid = eid, hp = hp, power = str,
          ai = Monster, name = show species, species = species }

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
  replicateM_ numEnemies $ do
    enemy <- makeEnemy
    saywords ["A", name enemy, "with", show (hp enemy),
      "HP is lurking in the darkness."]
    storeEntity enemy
  say "You bare your sword and leap into the fray."
  storeEntity playerEntity
  playTurn

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

promptYN str = do
  r <- prompt str
  case r of
    [] -> return True
    ('y':_) -> return True
    ('Y':_) -> return True
    _ -> return False

main :: IO ()
main = do
  newGame
  retry <- promptYN "Play again? [Yn] "
  if retry then main else return ()
