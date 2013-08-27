module AI where

import System.Random (randomRIO)

import GameTypes
import World
import Entity
import Room
import UI
import Rand
import Describe
import Action
import Coords

tick :: Entity -> Game ()
tick self = do
  selves <- getByEID (eid self)
  others <- anyOpponent self
  case (selves, others) of
    (Just self, Just other) | hp self > 0 -> do
      events <- runAI self other
      say $ describe (Tick :=> events)
    _ -> return ()

anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = hp e > 0 && eid e /= eid self

leaveGame e = do
  entities %= const [e]
  return [Walk :& [Agent e, WhichWay Up], Lose :& [Patient e]]

parseInstr "fight" = Attack
parseInstr "escape" = Goto
parseInstr _ = Rest

runAI :: Entity -> Entity -> Game [Event]
runAI player defender | isPlayer player = do
  move <- liftIO (prompt "[fight/escape] > ")
  case parseInstr move of
    Attack -> attack player defender
    Goto -> leaveGame player
    _ -> do
      saywords ["You don't know how to", move ++ "!"]
      runAI player defender
runAI self other = attack self other

setAI entity newType = do
  let oldType = aiType (ai entity)
      newAI = (ai entity) { aiType = newType }
  updateEntity $ entity { ai = newAI }

attack attacker defender = dealDamage attacker defender

dealDamage :: Entity -> Entity -> Game [Event]
dealDamage attacker defender = do
  let p = power attacker
  amount <- anyIn (p, p + 3)
  let newHP = hp defender - amount
      injured = defender { hp = newHP }
  updateEntity injured
  let d = TakeDamage :& [Agent attacker, Patient defender, ByAmount amount]
      n = NearDeath :& [Patient defender]
      x = Die :& [Patient defender]
      neardeath = newHP <= 10
      dead = newHP <= 0
      events = d : if dead then [x] else if neardeath then [n] else []
  if dead then setAI injured Inert else return ()
  return events

tellVictory :: Entity -> Game ()
tellVictory e = announce (Win :& [Patient e])
