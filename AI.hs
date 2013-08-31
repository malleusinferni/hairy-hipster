{-# LANGUAGE RecordWildCards #-}
module AI where

import Control.Applicative ((<*>))

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
      Just self -> do
        action <- self `respondTo` Tick
        self `runAI` action
      _ -> return []

runAI :: Entity -> Action -> Game [Event]
runAI self Attack = do
  others <- anyOpponent self
  case others of
    Just defender -> self `dealDamage` defender
    Nothing -> return [Fail :& [Tried Attack]]
runAI self (Go dir) = do
  exit <- findExitFrom (location self) dir
  case exit of
    Nothing -> do
      announce [subj self, "can't go", describe dir]
      return []
    Just door -> do
      dest <- self `traverseExit` door
      return [Walk :& [Patient self, WhichWay dir, Via door],
              Walk :& [Patient self, Into dest]]
runAI self Rest = do
  rec <- fuzz (hp self `quot` 10)
  updateEntity $ self { hp = hp self + rec }
  return [Heal :& [Patient self]]
runAI _ _ = return [] -- [NothingHappens :& []]

anyOpponent :: Entity -> Game (Maybe Entity)
anyOpponent self = getEntitiesWhere test >>= anyOf
  where test e = isAlive e &&
          eid e /= eid self &&
            location e == location self

leaveGame :: Entity -> Game [Event]
leaveGame e = do
  entities %= const [e]
  return [Walk :& [Agent e, WhichWay Up], Lose :& [Patient e]]

parseInstr :: String -> Action
parseInstr "" = Attack
parseInstr "fight" = Attack
parseInstr "up" = Go Up
parseInstr "down" = Go Down
parseInstr "north" = Go North
parseInstr "south" = Go South
parseInstr "east" = Go East
parseInstr "west" = Go West
parseInstr "ne" = Go Northeast
parseInstr "nw" = Go Northwest
parseInstr "se" = Go Southeast
parseInstr "sw" = Go Southwest
parseInstr _ = Rest

playerAI, actorAI, objectAI, inertAI :: EID -> AI

playerAI = playerMM `inherit` Just actorAI
actorAI = actorMM `inherit` Just objectAI
objectAI = objectMM `inherit` Just inertAI
inertAI = inertMM `inherit` Nothing

inherit :: (EID -> Responder) -> Maybe (EID -> AI) -> EID -> AI
inherit resp super' entity = AI{..}
  where methods = makeMethodMap []
        super = super' <*> Just entity
        ifMissing = resp entity

playerMM, actorMM, objectMM, inertMM :: EID -> Responder
playerMM eid (Tick) = do
  move <- liftIO (prompt "> ")
  let r = parseInstr move
  case r of
    Attack -> return r
    Go _ -> return r
    Rest -> return r
    _ -> do
      saywords ["You don't know how to", move ++ "!"]
      playerMM eid Tick
playerMM eid t = actorMM eid t

actorMM _ (Tick) = return Attack
actorMM eid t = objectMM eid t

objectMM = inertMM

inertMM _ _ = return Rest

makeCorpse :: Entity -> Game Entity
makeCorpse e@(Entity{..}) = return $ e { ai = inertAI eid }

attackPower :: Entity -> Game Int
attackPower = fuzz . power

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
