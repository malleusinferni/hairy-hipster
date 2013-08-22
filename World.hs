module World (
  World(..), Game(..),
  (%=), ($=), storeEntity, updateEntity, getEntities,
  makeWorld, nextID,
  say, saywords,
  asks, liftIO, runReaderT
  ) where

import Data.IORef
import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Entity
import Room

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

streamIDs :: Int -> IO (IO ID)
streamIDs n = do
  ref <- newEmptyMVar
  forkIO (mapM_ (putMVar ref) [n ..])
  return (fmap EID $ takeMVar ref)

nextID :: Game ID
nextID = liftIO =<< asks getID

makeWorld :: IO World
makeWorld = do
  stream <- streamIDs 0
  ref <- newIORef []
  let well = Door { doortype = "well", doordir = Down }
      outside = Room { placename = "outside", doors = [(well, inside)] }
      inside = Room { placename = "inside", doors = [] }
  return World { getID = stream, entities = ref,
    locations = [outside, inside] }

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

updateEntity e = entities %= (map $ \i -> if e == i then e else i)

getEntities :: Game [Entity]
getEntities = asks entities >>= liftIO . readIORef
