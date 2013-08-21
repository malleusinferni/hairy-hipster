data Entity = Entity {
    eid :: ID,
    hp :: Int,
    name :: String
  } deriving (Eq, Ord, Show)
data ID = Player | EID Int deriving (Eq, Ord, Show)

playTurn entities = do
  weakEntities <- mapM (takeDamage 5) entities
  let [player] = filter ((== Player) . eid) weakEntities
  putStrLn (tellHealth player)
  if hp player > 0
     then playTurn weakEntities
     else return ()

takeDamage amount e = do
  putStrLn ("Ouch! " ++ name e ++ " takes " ++ show amount ++ " damage.")
  return (e { hp = hp e - amount })

tellHealth (Entity _ hp _)
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = playTurn [player]
  where player = Entity { eid = Player, hp = 25, name = "Player" }
