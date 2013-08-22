module AI where

import System.Random (randomRIO)

import World
import Entity
import Room
import UI

runAI player@(Entity { ai = Player }) (defender : bystanders) = do
  yn <- liftIO . promptYN $ "Attack the " ++ name defender ++ "? [Yn] "
  if yn
     then do
       defender <- dealDamage player defender
       tellHealth defender
       return $ filter stillAlive ((defender : bystanders) ++ [player])
     else do
       say "Climbing back up the well, you escape with your life..."
       return [player]
runAI attacker (defender : bystanders) = do
  defender <- dealDamage attacker defender
  tellHealth defender
  return $ filter stillAlive ((defender : bystanders) ++ [attacker])
runAI attacker _ = do
  saywords ["The", name attacker, "emerges victorious!"]
  return [attacker]

dealDamage attacker defender = do
  let p = power attacker
  amount <- liftIO $ randomRIO (p, p + 3)
  saywords ["The", name attacker, "hits the", name defender,
    "for", show amount, "damage!"]
  return defender { hp = hp defender - amount }

tellHealth :: Entity -> Game ()
tellHealth (Entity { hp = hp, ai = Player })
  | hp > 10 = say "You feel fine."
  | hp > 0 = say "You feel woozy from blood loss."
  | otherwise = say "Your hit points dwindle to zero. You perish!"
tellHealth npc@(Entity { hp = hp })
  | hp <= 0 = saywords ["The", name npc, "collapses in a pool of blood."]
tellHealth _ = return ()
