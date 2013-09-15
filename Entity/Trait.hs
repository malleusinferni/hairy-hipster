module Entity.Trait where

import qualified Data.Map.Strict as M

import Entity.Value

type Map = M.Map Key Value

get :: Key -> Map -> Value
get = M.findWithDefault Nil

put :: Key -> Value -> Map -> Map
put = M.insert

make :: [(Key, Value)] -> Map
make list
  | length (M.keys m) < numTraits = error msg
  | otherwise = m
  where m = M.fromList list
        msg = "Uninitialized fields in " ++ show list

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
