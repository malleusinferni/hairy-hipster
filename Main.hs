data Entity = Player { hp :: Int }

playTurn (Player hp)
  | hp > 0 = "You feel fine."
  | otherwise = "Your hit points dwindle to zero. You perish!"

main = putStrLn (playTurn (Player 0))
