module AI where

import System.Random (randomRIO)

import World
import Entity
import Room
import UI
import Rand
import Describe
import Action

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
  let ask = concat ["Attack ", accusative (name defender), "? [Yn] "]
  yn <- liftIO (promptYN ask)
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
  announce $ Damage attacker (verb "hit") defender amount
  return defender { hp = hp defender - amount }

tellVictory :: Entity -> Game ()
tellVictory = announce . Win

tellHealth :: Entity -> Game ()
tellHealth (Entity { hp = hp, ai = Player })
  | hp > 10 = say "You feel fine."
  | hp > 0 = say "You feel woozy from blood loss."
  | otherwise = say "Your hit points dwindle to zero. You perish!"
tellHealth npc@(Entity { hp = hp })
  | hp <= 0 = announce (Perish npc)
tellHealth _ = return ()
