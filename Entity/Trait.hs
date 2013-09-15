module Entity.Trait where

import qualified Data.Map.Strict as M

import Entity.Value

type Map = M.Map Key Value

get :: Key -> Map -> Value
get = M.findWithDefault Nil

put :: Key -> Value -> Map -> Map
put = M.insert

make :: [(Key, Value)] -> Map
make = M.fromList

replace :: Key -> Value -> Map -> Map -> Map
replace k v m e
  | v `typeCheck` get k m = put k v m
  | otherwise = e

data Key = Location
         | Species
         | HitPoints
         | Strength
         | IsPlayer
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

numTraits = fromEnum (maxBound :: Key) + 1
