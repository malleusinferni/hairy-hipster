{-# LANGUAGE RecordWildCards #-}
module World (
  World(..), Game(..),
  (%=), ($=), storeEntity, updateEntity, getEntities,
  anyEntityExcept, getEntitiesWhere, getByID,
  makeWorld, nextID, anyRoom,
  say, saywords, announce,
  asks, liftIO, runReaderT
  ) where

import Data.List (find, delete)
import Data.IORef
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Entity
import Room
import Rand
import Describe
import Coords

data World = World {
    getID :: IO ID,
    entities :: IORef [Entity],
    locations :: [Room]
  }

type Game a = ReaderT World IO a

say :: String -> Game ()
say = liftIO . putStrLn

saywords :: [String] -> Game ()
saywords = say . unwords

announce :: (Effable a) => a -> Game ()
announce d = say (sentence d ".")

streamIDs :: Int -> IO (IO ID)
streamIDs n = do
  ref <- newEmptyMVar
  forkIO (mapM_ (putMVar ref) [n ..])
  return (fmap EID $ takeMVar ref)

nextID :: Game ID
nextID = liftIO =<< asks getID

makeWorld :: IO World
makeWorld = do
  getID <- streamIDs 0
  entities <- newIORef []
  locations <- makeMap 1
  return World{..}

makeMap :: Int -> IO [Room]
makeMap gridSize = do
  let outside = Room { exits = [(Down, inside)], onGrid = ZYX 1 0 0 }
      inside = Room { exits = [(Up, outside)], onGrid = ZYX 0 0 0 }
  return [outside, inside]

type Selector a = World -> IORef a

(%=) :: Selector a -> (a -> a) -> Game ()
sel %= action = do
  ref <- asks sel
  liftIO $ modifyIORef' ref action

($=) :: Selector a -> a -> Game ()
sel $= value = do
  ref <- asks sel
  liftIO $ writeIORef ref value

storeEntity e = entities %= (e:)

updateEntity e = entities %= ((e :) . delete e)

getEntities :: Game [Entity]
getEntities = asks entities >>= liftIO . readIORef

getEntitiesWhere :: (Entity -> Bool) -> Game [Entity]
getEntitiesWhere test = filter test `fmap` getEntities

getByID :: ID -> Game (Maybe Entity)
getByID anid = find ((== anid) . eid) `fmap` getEntities

anyEntityExcept :: Entity -> Game (Maybe Entity)
anyEntityExcept self = getEntities >>= anyOf . filter (/= self)

anyRoom :: Game Room
anyRoom = do
  [outside, inside] <- asks locations
  return inside
