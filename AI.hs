module AI where

import System.Random (randomRIO)

import GameTypes
import World
import Entity
import UI
import Rand
import Describe
import Action
import Coords

tick :: Entity -> Game ()
tick self = do
  let instr = getResponder (Tick) self
  selves <- getByEID (eid self)
  others <- anyOpponent self
  case (selves, others, instr) of
    (Just self, Just other, Just trigger) | hp self > 0 -> do
      action <- trigger (Tick)
      events <- runAI action self other
      say $ describe (Tick :=> events)
    _ -> return ()

anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = hp e > 0 && eid e /= eid self

leaveGame :: Entity -> Game [Event]
leaveGame e = do
  entities %= const [e]
  return [Walk :& [Agent e, WhichWay Up], Lose :& [Patient e]]

parseInstr :: String -> Action
parseInstr "fight" = Attack
parseInstr "escape" = Goto
parseInstr _ = Rest

playerTick :: Responder
playerTick (Tick) = do
  move <- liftIO (prompt "[fight/escape] > ")
  let r = parseInstr move
  if elem r [Attack, Goto]
     then return r
     else do
      saywords ["You don't know how to", move ++ "!"]
      playerTick Tick

monsterTick (Tick) = return Attack

runAI :: Action -> Entity -> Entity -> Game [Event]
runAI Attack self other = attack self other
runAI _ _ _ = return [NothingHappens :& []]

makeCorpse :: Entity -> Game Entity
makeCorpse e = do
  let oldAI = ai e
      newAI = removeHooks [Tick] oldAI
  return $ e { ai = newAI }

attack :: Entity -> Entity -> Game [Event]
attack attacker defender = dealDamage attacker defender

dealDamage :: Entity -> Entity -> Game [Event]
dealDamage attacker defender = do
  let p = power attacker
  amount <- anyIn (p, p + 3)
  let newHP = hp defender - amount
      injured = defender { hp = newHP }
      d = TakeDamage :& [Agent attacker, Patient injured, ByAmount amount]
      n = NearDeath :& [Patient injured]
      x = Die :& [Patient injured]
      neardeath = newHP <= 10
      dead = newHP <= 0
      events
        | dead = [d, x]
        | neardeath = [d, n]
        | otherwise = [d]
  if dead
     then makeCorpse injured >>= updateEntity
     else updateEntity injured
  return events

tellVictory :: Entity -> Game ()
tellVictory e = announce (Win :& [Patient e])
