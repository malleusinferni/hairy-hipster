module Rand where

import System.Random
import Control.Monad.IO.Class
import Control.Monad (replicateM, liftM)
import Data.List (sort)

anyOf :: (MonadIO m) => [a] -> m (Maybe a)
anyOf [] = return Nothing
anyOf xs = do
  idx <- anyIn (0, length xs - 1)
  return $ Just (xs !! idx)

anyIn :: (MonadIO m, Random a) => (a, a) -> m a
anyIn = liftIO . randomRIO

fuzz :: (MonadIO m, Integral a) => a -> m a
fuzz n = fix `liftM` roll 2
  where split = 2.0 * fromIntegral n
        fix n = round $ n * split

avg :: (Fractional a) => [a] -> a
avg [] = 0
avg xs = sum xs / fromIntegral (length xs)

rolls :: (Random n, MonadIO m) => Int -> m [n]
rolls n = replicateM n $ liftIO randomIO

roll :: (MonadIO m) => Int -> m Float
roll n
  | n < 1 = return 0.5
  | otherwise = avg `liftM` rolls n

rollDrop :: (MonadIO m) => Int -> Int -> m Float
rollDrop n d
  | n > d && d > 0 = do
    rs <- rolls n
    return $ (avg . take (n - d) . sort) rs
  | otherwise = roll n

bestOf :: (Monad m, Ord b) => Int -> m b -> m b
bestOf n k = do
  vals <- replicateM n k
  return (maximum vals)
