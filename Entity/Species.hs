{-# LANGUAGE RecordWildCards #-}
module Entity.Species where

import Table

data Species = Species
  { speciesName :: String
  , minHeight :: Int
  , maxHeight :: Int
  } deriving (Eq, Show, Ord)

instance Tabular Species where
  readRecord = do
    speciesName <- copyField "Name"
    minHeight <- readField "Min height"
    maxHeight <- readField "Max height"
    return Species{..}
