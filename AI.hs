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

tick :: Entity -> Game EventReport
tick self = do
  selves <- getByEID (eid self)
  fmap (Tick :=>) $
    case selves of
      Just self | isAlive self -> do
        action <- self `respondTo` Tick
        self `runAI` action
      _ -> return []

runAI :: Entity -> Action -> Game [Event]
runAI self Attack = do
  others <- anyOpponent self
  case others of
    Just defender -> self `dealDamage` defender
    Nothing -> return [Stand :& []]
runAI _ _ = return [NothingHappens :& []]

anyOpponent :: Entity -> Game (Maybe Entity)
anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = isAlive e && eid e /= eid self

leaveGame :: Entity -> Game [Event]
leaveGame e = do
  entities %= const [e]
  return [Walk :& [Agent e, WhichWay Up], Lose :& [Patient e]]

parseInstr :: String -> Action
parseInstr "" = Attack
parseInstr "fight" = Attack
parseInstr "escape" = Goto
parseInstr _ = Rest

playerAI, actorAI, objectAI, inertAI :: EID -> AI

playerAI entity = AI{..}
  where super = Just $ actorAI entity
        ifMissing = playerMM
        methods = makeMethodMap []

actorAI entity = AI{..}
  where super = Just $ objectAI entity
        ifMissing = actorMM
        methods = makeMethodMap []

objectAI entity = AI{..}
  where super = Just $ inertAI entity
        ifMissing = objectMM
        methods = makeMethodMap []

inertAI entity = AI{..}
  where super = Nothing
        ifMissing = inertMM
        methods = makeMethodMap []

playerMM :: Responder
playerMM (Tick) = do
  move <- liftIO (prompt "[fight/escape] > ")
  let r = parseInstr move
  if r `elem` [Attack, Goto]
     then return r
     else do
      saywords ["You don't know how to", move ++ "!"]
      playerMM Tick
playerMM t = actorMM t

actorMM :: Responder
actorMM (Tick) = return Attack
actorMM t = objectMM t

objectMM :: Responder
objectMM = inertMM

inertMM :: Responder
inertMM _ = return Rest

makeCorpse :: Entity -> Game Entity
makeCorpse e@(Entity{..}) = return $ e { ai = popAI ai }

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
