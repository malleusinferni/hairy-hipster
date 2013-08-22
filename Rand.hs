module Rand where

import System.Random
import Control.Monad.IO.Class

anyOf :: (MonadIO m) => [a] -> m (Maybe a)
anyOf [] = return Nothing
anyOf xs = do
  idx <- anyIn (0, length xs - 1)
  return $ Just (xs !! idx)

anyIn :: (MonadIO m, Random a) => (a, a) -> m a
anyIn = liftIO . randomRIO
