data Entity = Entity { eid :: ID, hp :: Int } deriving (Eq, Ord, Show)
data ID = Player | EID Int deriving (Eq, Ord, Show)

playTurn player = do
  weakenedPlayer <- takeDamage player 5
  putStrLn (tellHealth weakenedPlayer)
  if hp weakenedPlayer > 0
     then playTurn weakenedPlayer
     else return ()

takeDamage e amount = do
  return (e { hp = hp e - amount })

tellHealth (Entity _ hp)
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = playTurn player
  where player = Entity { eid = Player, hp = 0 }
