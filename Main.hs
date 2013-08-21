import System.IO (stdout, hFlush)
import System.Random (randomRIO)
import Control.Monad (replicateM)

import Entity

playerEntity = Entity Player 25 Merovingian 8 "player"

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

randomEnemy = do
  let minSpecies = fromEnum (minBound :: Species)
      maxSpecies = fromEnum (maxBound :: Species)
  rid <- randomRIO (0, maxBound)
  rhp <- randomRIO (5, 35)
  str <- randomRIO (5, 15)
  idx <- randomRIO (minSpecies, maxSpecies)
  let spc = toEnum idx :: Species
      nam = show spc
  return Entity { eid = EID rid, hp = rhp, name = nam,
    power = str, species = spc }

newGame = do
  putStrLn "You climb down the well."
  numEnemies <- randomRIO (1, 5)
  enemies <- replicateM numEnemies $ do
    enemy <- randomEnemy
    putStrLn $ unwords ["A", name enemy, "with", show (hp enemy),
      "HP is lurking in the darkness."]
    return enemy
  putStrLn "You bare your sword and leap into the fray."
  playTurn (playerEntity : enemies)

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
