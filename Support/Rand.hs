module Support.Rand where

import System.Random
import Control.Monad.IO.Class
import Control.Monad (replicateM, liftM)
import Data.List (sort)

-- Random selection from a list
-- Uniform distribution
anyOf :: (MonadIO m) => [a] -> m (Maybe a)
anyOf [] = return Nothing
anyOf xs = do
  idx <- anyIn (0, length xs - 1)
  return $ Just (xs !! idx)

-- Random value in arbitrary range
-- Uniform distribution
anyIn :: (MonadIO m, Random a) => (a, a) -> m a
anyIn = liftIO . randomRIO

-- Random value in range (0, n + n)
-- Normal distribution
fuzz :: (MonadIO m, Integral a, Random a) => a -> m a
fuzz n = n +/- n

-- Random value in range (n - k, n + k)
-- Normal distribution
(+/-) :: (MonadIO m, Num a, Random a) => a -> a -> m a
n +/- k = do
  ks <- rollR 2 (n, k)
  return $ (n - k) + sum ks

-- Mean value of a list
avg :: (Fractional a) => [a] -> a
avg [] = error "Mean value of empty list is undefined"
avg xs = sum xs / realToFrac (length xs)

-- N random values in arbitrary range
-- Uniform distribution
rollR :: (MonadIO m, Random a) => Int -> (a, a) -> m [a]
rollR n = replicateM n . anyIn

rolls :: (Random n, Num n, MonadIO m) => Int -> m [n]
rolls n = rollR n (0, 1)

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
