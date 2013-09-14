module Entity.Material where

data Material = Flesh
              | Steel
              | Carapace
              | Air
              | Grass
              | Stone
              | Masonry
              | Dirt
              | Sand
              | Blood
              | Sandstone
              | Wood
  deriving (Eq, Show, Ord, Enum)
