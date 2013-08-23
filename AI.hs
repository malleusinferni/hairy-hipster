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
    (Just self, Just other) | hp self > 0 -> do
      (Report v outcomes) <- runAI self other
      announce v
      mapM_ announce outcomes
    _ -> return ()

anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = hp e > 0 && eid e /= eid self

leaveGame e = do
  entities $= [e]
  let v = VI Goto e
      o = [RunAway e]
  return (Report v o)

runAI player@(Entity { ai = Player }) defender = do
  let ask = concat ["Attack ", accusative (name defender), "? [Yn] "]
  yn <- liftIO (promptYN ask)
  if yn
     then attack player defender
     else leaveGame player
runAI self other = attack self other

attack attacker defender = do
  let event = VT Attack attacker defender
  (defender, damage) <- dealDamage attacker defender
  updateEntity defender
  return (Report event (damage : tellHealth defender))

dealDamage attacker defender = do
  let p = power attacker
  amount <- anyIn (p, p + 3)
  let injured = defender { hp = hp defender - amount }
  updateEntity injured
  return (injured, TakeDamage injured amount)

tellVictory :: Entity -> Game ()
tellVictory = announce . Win

tellHealth :: Entity -> [Outcome]
tellHealth e@(Entity { hp = hp }) =
  concat [if hp <= 10 then [NearDeath e] else [],
          if hp <= 0 then [Die e] else []]
