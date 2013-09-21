{-# LANGUAGE RecordWildCards #-}
module Entity.Species where

import Grammar.Atom

import Table
import Describe

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

instance Effable Species where
  describe = fromString . speciesName

-- Size in inches
sizeRangeFor :: Species -> (Int, Int)
sizeRangeFor species = (minHeight species, maxHeight species)
