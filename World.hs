{-# LANGUAGE RecordWildCards #-}
module World
  ( (%=)
  , ($=)
  , storeEntity
  , updateEntity
  , getEntities
  , anyEntityExcept
  , getEntitiesWhere
  , getByEID
  , makeWorld
  , nextEID
  , anyRoom
  , findExitFrom
  , traverseExit
  , say
  , saywords
  , announce
  , asks
  , liftIO
  , runReaderT
  ) where

import Data.List (find, delete)
import qualified Data.Map.Lazy as M
import Data.IORef
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import GameTypes
import Entity ()
import Rand
import Describe
import Coords
import Table

import AlphaDungeon

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

makeWorld :: IO World
makeWorld = do
  getEID <- streamIDs 0
  entities <- newIORef []
  speciesData <- readTSVFile "species.tsv"
  locations <- makeMap
  return World{..}

makeMap :: IO LevelMap
makeMap = return (roomMap, corMap)
  where (rooms, corridors) = alphaDungeon
        byCoords r = (onGrid r, r)
        byExit r = (r, findExits r corridors)
        roomMap = M.fromList $ map byCoords rooms
        corMap = M.fromList $ map (byExit . onGrid) rooms

findExits loc cors = filter test cors
  where test (Corridor { endpoints = (s, e) }) = s == loc || e == loc

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
  (rooms, _) <- asks locations
  Just r <- anyOf $ filter ((/= 0) . xy . onGrid) (M.elems rooms)
  return r

findExitFrom :: Coords -> Cardinal -> Game (Maybe Corridor)
findExitFrom here dir = do
  let there = dirToCoords dir + here
  (rooms, exits) <- asks locations
  return $ do
    xs <- M.lookup here exits
    let pair = (here, there)
        riap = (there, here)
        test (Corridor { endpoints = e }) = e `elem` [pair, riap]
    find test xs

traverseExit :: Entity -> Corridor -> Game Room
traverseExit self door = do
  (rooms, exits) <- asks locations
  let (a, b) = endpoints door
      (here, there)
        | location self == a = (a, b)
        | otherwise = (b, a)
      Just r = M.lookup there rooms
  updateEntity (self { location = there })
  return r
