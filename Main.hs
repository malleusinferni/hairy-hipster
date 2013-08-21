data Entity = Entity {
    eid :: ID,
    hp :: Int,
    name :: String
  } deriving (Eq, Ord, Show)
data ID = Player | EID Int deriving (Eq, Ord, Show)

playTurn (attacker : defender : bystanders) = do
  defender <- takeDamage 5 attacker defender
  let undamaged = bystanders ++ [attacker]
      survivors
        | hp defender > 0 = defender : undamaged
        | otherwise = undamaged
  if length survivors > 1
     then playTurn survivors
     else putStrLn $ name (head survivors) ++ " wins!"

takeDamage amount attacker defender = do
  putStrLn $ unwords ["Ouch!", name attacker, "hits", name defender,
                      "for", show amount, "damage!"]
  let def = defender { hp = hp defender - amount }
  if eid def == Player
     then putStrLn (tellHealth def)
     else return ()
  return def

tellHealth (Entity _ hp _)
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = playTurn [player, enemy]
  where player = Entity { eid = Player, hp = 25, name = "Player" }
        enemy = Entity { eid = EID 0, hp = 14, name = "Goblin" }
