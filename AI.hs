module AI where

import System.Random (randomRIO)

import World
import Entity
import Room
import UI
import Rand

tick :: Entity -> Game ()
tick self = do
  selves <- getByID (eid self)
  others <- anyOpponent self
  case (selves, others) of
    (Just self, Just other) | hp self > 0 -> runAI self other
    _ -> return ()

anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = hp e > 0 && eid e /= eid self

runAI player@(Entity { ai = Player }) defender = do
  yn <- liftIO . promptYN $ "Attack the " ++ name defender ++ "? [Yn] "
  if yn
     then attack player defender
     else do
       say "Climbing back up the well, you escape with your life..."
       entities $= [player]
runAI self other = attack self other

attack attacker defender = do
  defender <- dealDamage attacker defender
  bystanders <- filter (/= defender) `fmap` getEntities
  tellHealth defender
  updateEntity defender

dealDamage attacker defender = do
  let p = power attacker
  amount <- anyIn (p, p + 3)
  saywords ["The", name attacker, "hits the", name defender,
    "for", show amount, "damage!"]
  return defender { hp = hp defender - amount }

tellVictory :: Entity -> Game ()
tellVictory (Entity { ai = Player }) = say "You emerge victorious!"
tellVictory npc = saywords ["The", name npc, "emerges victorious!"]

tellHealth :: Entity -> Game ()
tellHealth (Entity { hp = hp, ai = Player })
  | hp > 10 = say "You feel fine."
  | hp > 0 = say "You feel woozy from blood loss."
  | otherwise = say "Your hit points dwindle to zero. You perish!"
tellHealth npc@(Entity { hp = hp })
  | hp <= 0 = saywords ["The", name npc, "collapses in a pool of blood."]
tellHealth _ = return ()
