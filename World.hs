{-# LANGUAGE RecordWildCards #-}
module World (
  (%=), ($=), storeEntity, updateEntity, getEntities,
  anyEntityExcept, getEntitiesWhere, getByEID,
  makeWorld, nextEID, nextRID, anyRoom,
  say, saywords, announce,
  asks, liftIO, runReaderT
  ) where

import Data.List (find, delete)
import Data.IORef
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import GameTypes
import Entity ()
import Rand
import Describe
import Coords

say :: String -> Game ()
say = liftIO . putStrLn

saywords :: [String] -> Game ()
saywords = say . unwords

announce :: (Effable a) => a -> Game ()
announce d = say (sentence d ".")

streamIDs :: Int -> IO (IO Int)
streamIDs n = do
  ref <- newEmptyMVar
  forkIO (mapM_ (putMVar ref) [n ..])
  return (takeMVar ref)

nextEID :: Game EID
nextEID = liftIO =<< asks getEID

nextRID :: Game RID
nextRID = liftIO =<< asks getRID

makeWorld :: IO World
makeWorld = do
  getEID <- streamIDs 0
  getRID <- streamIDs 0
  entities <- newIORef []
  locations <- makeMap getRID 1
  return World{..}

makeMap :: IO RID -> Int -> IO [Room]
makeMap stream _gridSize = do
  orid <- stream
  irid <- stream
  let outside = Room { rid = orid, exits = [(Down, irid)], onGrid = zyx 1 0 0 }
      inside = Room { rid = irid, exits = [(Up, orid)], onGrid = zyx 0 0 0 }
  return [outside, inside]

(%=) :: Selector a -> (a -> a) -> Game ()
sel %= action = do
  ref <- asks sel
  liftIO $ modifyIORef' ref action

($=) :: Selector a -> a -> Game ()
sel $= value = do
  ref <- asks sel
  liftIO $ writeIORef ref value

storeEntity, updateEntity :: Entity -> Game ()
storeEntity e = entities %= (e:)
updateEntity e = entities %= ((e :) . delete e)

getEntities :: Game [Entity]
getEntities = asks entities >>= liftIO . readIORef

getEntitiesWhere :: (Entity -> Bool) -> Game [Entity]
getEntitiesWhere test = filter test `fmap` getEntities

getByEID :: EID -> Game (Maybe Entity)
getByEID anid = find ((== anid) . eid) `fmap` getEntities

anyEntityExcept :: Entity -> Game (Maybe Entity)
anyEntityExcept self = getEntities >>= anyOf . filter (/= self)

anyRoom :: Game Room
anyRoom = do
  [_outside, inside] <- asks locations
  return inside
