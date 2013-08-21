data Entity = Entity { eid :: ID, hp :: Int } deriving (Eq, Ord, Show)
data ID = Player | EID Int deriving (Eq, Ord, Show)

playTurn = tellHealth . hp

tellHealth hp
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = putStrLn (playTurn player)
  where player = Entity { eid = Player, hp = 0 }
