{-# LANGUAGE RecordWildCards #-}
module AI
  ( tick
  , makeAI
  ) where

import Control.Applicative ((<*>))
import Control.Monad (when)

import UI
import Describe

import Support.Coords
import Support.Rand

import Entity.Core
import Entity.Value as V
import qualified Entity.Trait as K

import World
import World.Core
import World.Entity

import AI.Trigger
import AI.Action
import AI.Event
import AI.Binding

import Event.Action

tick :: EID -> Game EventReport
tick eid = do
  self <- getByEID eid
  fmap (Tick :=>) $ do
    action <- eid `respondTo` Tick
    self `runAI` action

respondTo :: EID -> Responder
respondTo eid t = do
  ai <- getAI eid
  ai `getMethod` t

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
      _ <- self `traverseExit` door
      traveler <- getByEID (eid self)
      comments <- viewRoom traveler
      return $ (Walk :& [Patient traveler, WhichWay dir, Via door]) : comments
runAI self Rest = do
  amount <- fuzz 5
  updateEntity self K.HitPoints (increase amount)
  return [Heal :& [Patient self]]
runAI _ _ = return [] -- [NothingHappens :& []]

anyOpponent :: Entity -> Game (Maybe Entity)
anyOpponent self = getEntitiesWhere (isOpponentOf self) >>= anyOf

parseInstr :: String -> Action
parseInstr "" = Attack
parseInstr "fight" = Attack
parseInstr "look" = Look
parseInstr "up" = Go Up
parseInstr "down" = Go Down
parseInstr "north" = Go North
parseInstr "south" = Go South
parseInstr "east" = Go East
parseInstr "west" = Go West
parseInstr "n" = Go North
parseInstr "s" = Go South
parseInstr "e" = Go East
parseInstr "w" = Go West
parseInstr "ne" = Go Northeast
parseInstr "nw" = Go Northwest
parseInstr "se" = Go Southeast
parseInstr "sw" = Go Southwest
parseInstr "rest" = Rest
parseInstr "r" = Rest
parseInstr _ = DoNothing

playerAI, actorAI, objectAI, inertAI :: EID -> AI

playerAI = playerMM `inherit` Just actorAI
actorAI = actorMM `inherit` Just objectAI
objectAI = objectMM `inherit` Just inertAI
inertAI = inertMM `inherit` Nothing

makeAI :: Bool -> EID -> AI
makeAI True entity = playerAI entity
makeAI False entity = actorAI entity

inherit :: (EID -> Responder) -> Maybe (EID -> AI) -> EID -> AI
inherit resp super' entity = Bind{..}
  where methods = makeMethodMap []
        super = super' <*> Just entity
        ifMissing = resp entity

playerMM, actorMM, objectMM, inertMM :: EID -> Responder
playerMM eid (Tick) = do
  self <- getByEID eid
  move <- liftIO (prompt "> ")
  let r = parseInstr move
  case r of
    Attack -> return r
    Go _ -> return r
    Rest -> return r
    Look -> do
      observations <- viewRoom self
      announce (Seen :=> observations)
      playerMM eid Tick -- Don't lose a turn
    _ -> do
      saywords ["You don't know how to", move ++ "!"]
      playerMM eid Tick
playerMM eid t = actorMM eid t

viewRoom :: Entity -> Game [Event]
viewRoom self = do
  let loc = location self
  Just room <- roomByLocation loc
  let view = Walk :& [Patient self, Into room]
  others <- getEntitiesWhere (isOpponentOf self)
  enemies <- mapM (viewEntity self) others
  doors <- describeExitsFrom self
  return (view : (enemies ++ doors))

viewEntity :: Entity -> Entity -> Game Event
viewEntity self other = return $ See :& [Agent self, Patient other]

actorMM _ (Tick) = return Attack
actorMM eid t = objectMM eid t

objectMM = inertMM

inertMM _ _ = return DoNothing

makeCorpse :: Entity -> Game ()
makeCorpse e = eid e `putAI` inertAI (eid e)

attackPower :: Entity -> Game Int
attackPower = fuzz . power

dealDamage :: Entity -> Entity -> Game [Event]
dealDamage attacker defender = do
  amount <- attackPower attacker
  updateEntity defender K.HitPoints (reduce amount)
  injured <- getByEID (eid defender)
  let d = TakeDamage :& [Agent attacker, Patient injured, ByAmount amount]
      n = NearDeath :& [Patient injured]
      x = Die :& [Patient injured]
      events
        | isDead injured = [d, x]
        | isNearDeath injured = [d, n]
        | otherwise = [d]
  when (isDead injured) $
    makeCorpse injured
  return events
