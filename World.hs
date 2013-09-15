{-# LANGUAGE RecordWildCards #-}
module World
  ( makeWorld
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

import Data.List (find)
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import World.Core
import World.Entity
import World.Location

import Entity.Core
import Entity.Value
import qualified Entity.Trait as T

import AI.Event
import Event.Action
import Support.Rand
import Support.Coords
import Describe
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
  bindings <- newIORef IM.empty
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

findExits loc = filter test
  where test (Corridor { endpoints = (s, e) }) = s == loc || e == loc

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
  updateEntity self T.Location (goto there)
  return r
