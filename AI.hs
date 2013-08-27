{-# LANGUAGE RecordWildCards #-}
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
    (Just self, Just other, Just trigger) | isAlive self -> do
      action <- trigger (Tick)
      events <- runAI action self other
      say $ describe (Tick :=> events)
    _ -> return ()

anyOpponent :: Entity -> Game (Maybe Entity)
anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = isAlive e && eid e /= eid self

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

monsterTick :: Responder
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

attackPower :: Entity -> Game Int
attackPower = anyIn . attackRangeFor

dealDamage :: Entity -> Entity -> Game [Event]
dealDamage attacker defender = do
  amount <- attackPower attacker
  let injured = defender { hp = hp defender - amount }
      d = TakeDamage :& [Agent attacker, Patient injured, ByAmount amount]
      n = NearDeath :& [Patient injured]
      x = Die :& [Patient injured]
      events
        | isDead injured = [d, x]
        | isNearDeath injured = [d, n]
        | otherwise = [d]
  if isDead injured
     then makeCorpse injured >>= updateEntity
     else updateEntity injured
  return events

tellVictory :: Entity -> Game ()
tellVictory e = announce (Win :& [Patient e])
