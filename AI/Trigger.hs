module AI.Trigger where

import Data.Hashable

-- Prompts to which an AI may (or may not) respond
data Trigger = Tick
             | Impacted Int
             | Pierced Int
             | Slashed Int
             | Burned Int
             | Seen
  deriving (Eq, Show)

instance Hashable Trigger where
  hash Tick = 0
  hash (Impacted _) = 1
  hash (Pierced _) = 2
  hash (Slashed _) = 3
  hash (Burned _) = 4
  hash Seen = 5
