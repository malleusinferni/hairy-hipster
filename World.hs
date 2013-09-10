{-# LANGUAGE RecordWildCards #-}
module World
  ( (%=)
  , ($=)
  , storeEntity
  , updateEntity
  , makeWorld
  , nextEID
  , anyRoom
  , roomByLocation
  , getExits
  , describeExitsFrom
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
import ActionTypes
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
  _ <- forkIO (mapM_ (putMVar ref) [n ..])
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

anyRoom :: Game Room
anyRoom = do
  (rooms, _) <- asks locations
  Just r <- anyOf $ filter ((/= 0) . xy . onGrid) (M.elems rooms)
  return r

roomByLocation loc = do
  (rooms, _) <- asks locations
  return $ M.lookup loc rooms

getExits :: Coords -> Game [Corridor]
getExits here = do
  (_, exits) <- asks locations
  return $ M.findWithDefault [] here exits

whereTo from via = (near, far)
  where (n, f) = endpoints via
        (near, far)
          | n == from = (n, f)
          | otherwise = (f, n)

findExitFrom :: Coords -> Cardinal -> Game (Maybe Corridor)
findExitFrom here dir = do
  exits <- getExits here
  return $ do
    let test e = dir == quadrant canon
          where (l, r) = endpoints e
                canon | l == here = r - l
                      | otherwise = l - r
    find test exits

describeExitsFrom :: Entity -> Game [Event]
describeExitsFrom self = do
  let loc = location self
  exits <- getExits loc
  forM exits $ \door -> do
    let (here, there) = whereTo loc door
        dir = quadrant $ quantize (there - here)
    return $ See :& [Agent self, Via door, OutOf here, WhichWay dir]

traverseExit :: Entity -> Corridor -> Game Room
traverseExit self door = do
  (rooms, _) <- asks locations
  let (_, there) = whereTo (location self) door
      Just r = M.lookup there rooms
  updateEntity (self { location = there })
  return r
