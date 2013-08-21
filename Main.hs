import System.IO (stdout, hFlush)
import System.Random (randomRIO)

import Entity

playerEntity = Entity Player 25 Merovingian "player"

playTurn (attacker : defender : bystanders) = do
  defender <- takeDamage 5 attacker defender
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

takeDamage amount attacker defender = do
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
  idx <- randomRIO (minSpecies, maxSpecies)
  let spc = toEnum idx :: Species
      nam = show spc
  return Entity { eid = EID rid, hp = rhp, name = nam, species = spc }

newGame = do
  enemy <- randomEnemy
  putStrLn "You climb down the well."
  putStrLn $ unwords ["A", name enemy, "with", show (hp enemy),
    "HP is lurking below."]
  playTurn [playerEntity, enemy]

main = do
  newGame
  putStr "Play again? [yn] "
  hFlush stdout
  response <- getLine
  case response of
    [] -> main
    ('y':_) -> main
    ('Y':_) -> main
    _ -> return ()
