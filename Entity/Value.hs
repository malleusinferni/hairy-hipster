{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Entity.Value where

import Support.Coords
import Entity.Species

data Value = I { intValue :: Int }
           | B { boolValue :: Bool }
           | S { speciesValue :: Species }
           | L { coordsValue :: Coords }
           | Nil
  deriving (Eq, Show)

class Valuable v where
  dyn :: v -> Value

instance Valuable Int where dyn = I
instance Valuable Bool where dyn = B
instance Valuable Coords where dyn = L
instance Valuable Species where dyn = S

(~>) :: (Valuable v) => k -> v -> (k, Value)
k ~> v = (k, dyn v)

typeCheck :: Value -> Value -> Bool
typeCheck = go
  where go (I _) (I _) = True
        go (B _) (B _) = True
        go (S _) (S _) = True
        go (L _) (L _) = True
        go Nil Nil = True
        go _ _ = False

reduce :: Int -> Value -> Value
reduce amt (I v) = I (v - amt)
reduce _ _ = error "\"Reduce\" only works on numbers"

increase :: Int -> Value -> Value
increase amt (I v) = I (v + amt)
increase _ _ = error "\"Increase\" only works on numbers"

goto :: Coords -> Value -> Value
goto new (L _) = L new
goto _ _ = error "\"Goto\" only works on locations"
