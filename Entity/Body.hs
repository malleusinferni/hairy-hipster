module Entity.Body where

import Entity.Material

data Body = Body
  { material :: Material
  , size :: Int
  } deriving (Eq, Show)
