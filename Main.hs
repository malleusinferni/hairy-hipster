import System.Random (randomRIO)

data Entity = Entity {
    eid :: ID,
    hp :: Int,
    species :: Species,
    name :: String
  } deriving (Eq, Ord, Show)

data Species = Shoggoth | Goblin | Merovingian
  deriving (Eq, Show, Ord, Enum)

data ID = Player | EID Int deriving (Eq, Ord, Show)

playerEntity = Entity Player 25 Merovingian "player"

playTurn (attacker : defender : bystanders) = do
  defender <- takeDamage 5 attacker defender
  let undamaged = bystanders ++ [attacker]
      survivors
        | hp defender > 0 = defender : undamaged
        | otherwise = undamaged
  if length survivors > 1
     then playTurn survivors
     else putStrLn $ unwords ["The", name (head survivors), "wins!"]

takeDamage amount attacker defender = do
  putStrLn $ unwords ["Ouch! The", name attacker, "hits the", name defender,
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
  rid <- randomRIO (0, maxBound)
  rhp <- randomRIO (5, 35)
  idx <- randomRIO (fromEnum Shoggoth, fromEnum Merovingian)
  let spc = toEnum idx :: Species
      nam = show spc
  return Entity { eid = EID rid, hp = rhp, name = nam, species = spc }

main = do
  enemy <- randomEnemy
  putStrLn "You climb down the well."
  putStrLn $ unwords ["A", name enemy, "with", show (hp enemy),
    "HP is lurking below."]
  playTurn [playerEntity, enemy]
