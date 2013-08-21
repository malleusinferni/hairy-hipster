import System.IO (stdout, hFlush)
import System.Random (randomRIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (replicateM)
import Control.Monad.Reader

import Entity

data World = World { getID :: IO ID, entities :: MVar [Entity] }

type Game a = ReaderT World IO a

playerEntity = Entity Player 25 Merovingian 8 "player"

say = liftIO . putStrLn
saywords = say . unwords

playTurn (attacker : defender : bystanders) = do
  defender <- dealDamage attacker defender
  let undamaged = bystanders ++ [attacker]
      survivors
        | hp defender > 0 = defender : undamaged
        | otherwise = undamaged
  case survivors of
    [] -> putStrLn "Nobody survives..."
    [Entity { eid = Player }] -> do
      putStrLn $ unwords ["The", name defender, "falls in combat!"]
      putStrLn "You escape with your life..."
    [npc] -> putStrLn $ unwords ["The", name npc, "has defeated you..."]
    multiple -> playTurn survivors

dealDamage attacker defender = do
  amount <- randomRIO (power attacker, power attacker + 3)
  putStrLn $ unwords ["The", name attacker, "hits the", name defender,
                      "for", show amount, "damage!"]
  let def = defender { hp = hp defender - amount }
  if eid def == Player
     then putStrLn (tellHealth def)
     else return ()
  return def

tellHealth (Entity { hp = hp })
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

streamIDs :: Int -> IO (IO ID)
streamIDs n = do
  ref <- newEmptyMVar
  forkIO (mapM_ (putMVar ref) [n ..])
  return (fmap EID $ takeMVar ref)

nextID :: Game ID
nextID = liftIO =<< asks getID

makeWorld = do
  stream <- streamIDs 0
  ref <- newMVar []
  return World { getID = stream, entities = ref }

sel %= action = do
  ref <- asks sel
  liftIO (modifyMVar ref action)

makeEnemy = do
  eid <- nextID
  entities %= \es -> do
    species <- randomSpecies
    hp <- randomRIO (5, 35)
    str <- randomRIO (5, 15)
    let enemy = Entity { eid = eid, hp = hp, power = str,
          name = show species, species = species }
    return (enemy : es, enemy)

randomSpecies = toEnum `fmap` randomRIO (low, high)
  where [low, high] = map fromEnum range
        range = [minBound, maxBound] :: [Species]

newGame = makeWorld >>= runReaderT playGame

playGame = do
  say "You climb down the well."
  numEnemies <- liftIO (randomRIO (1, 5))
  enemies <- replicateM numEnemies $ do
    enemy <- makeEnemy
    saywords ["A", name enemy, "with", show (hp enemy),
      "HP is lurking in the darkness."]
    return enemy
  say "You bare your sword and leap into the fray."
  liftIO $ playTurn (playerEntity : enemies)

prompt str = do
  putStr str
  hFlush stdout
  getLine

main = do
  newGame
  response <- prompt "Play again? [yn] "
  case response of
    [] -> main
    ('y':_) -> main
    ('Y':_) -> main
    _ -> return ()
