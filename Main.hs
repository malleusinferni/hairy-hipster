data Entity = Player { hp :: Int }

playTurn = tellHealth . hp

tellHealth hp
  | hp > 10 = "You feel fine."
  | hp > 0 = "You feel woozy from blood loss."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = putStrLn (playTurn (Player 0))
