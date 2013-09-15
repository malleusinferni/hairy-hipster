{-# LANGUAGE RecordWildCards #-}
module Entity.Species where

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

instance Nominable Species where
  name s = Noun (describe s) (describe s ++ "'s") (describe s) False

instance Effable Species where
  describe = speciesName

-- Size in inches
sizeRangeFor :: Species -> (Int, Int)
sizeRangeFor species = (minHeight species, maxHeight species)
